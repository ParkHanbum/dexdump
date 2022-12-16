#include "ir_builder.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/ValueSymbolTable.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/Cloning.h"



using namespace llvm;
using namespace std;


namespace art {

extern FILE *gOutFile;

namespace llvm {

struct LocalInfo {
  LocalInfo() = default;

  const char *name_ = nullptr;       // E.g., list.  It can be nullptr if unknown.
  const char *descriptor_ = nullptr; // E.g., Ljava/util/LinkedList;
  const char *signature_ = nullptr;  // E.g., java.util.LinkedList<java.lang.Integer>
  uint32_t start_address_ = 0;       // PC location where the local is first defined.
  uint32_t end_address_ = 0;         // PC location where the local is no longer defined.
  uint16_t reg_ = 0;                 // Dex register which stores the values.
  bool is_live_ = false;             // Is the local defined and live.
  ::llvm::Value *val = nullptr;      // keep argument llvm value
};



// keep all local registers
std::vector<LocalInfo> local_in_reg;

bool debug = true;

::llvm::PointerType *java_object_type_;
::llvm::PointerType *java_method_type_;
::llvm::PointerType *java_thread_type_;

::llvm::LLVMContext *ctx;
::llvm::Module *gMod;
::llvm::Function *func_;
::llvm::BasicBlock *bb_;
std::string symbol_;
::llvm::IRBuilder<> *Builder;

std::unordered_map<u4, ::llvm::Function *> funcMap;
std::unordered_map<u4, const ClassAccessor::Method &> methodMap;

::llvm::GlobalVariable::LinkageTypes internal = ::llvm::GlobalVariable::LinkageTypes::InternalLinkage;
::llvm::GlobalVariable::LinkageTypes external = ::llvm::GlobalVariable::LinkageTypes::ExternalLinkage;

enum JType {
  kVoid,
  kBoolean,
  kByte,
  kChar,
  kShort,
  kInt,
  kLong,
  kFloat,
  kDouble,
  kObject,
  MAX_JTYPE
};

::llvm::Type *getJVoidTy() {
  return Type::getVoidTy(*ctx);
}

::llvm::IntegerType *getJBooleanTy() {
  return Type::getInt8Ty(*ctx);
}

::llvm::IntegerType *getJByteTy() {
  return Type::getInt8Ty(*ctx);
}

::llvm::IntegerType *getJCharTy() {
  return Type::getInt16Ty(*ctx);
}

::llvm::IntegerType *getJShortTy() {
  return Type::getInt16Ty(*ctx);
}

::llvm::IntegerType *getJIntTy() {
  return Type::getInt32Ty(*ctx);
}

::llvm::IntegerType *getJLongTy() {
  return Type::getInt64Ty(*ctx);
}

::llvm::Type *getJFloatTy() {
  return Type::getFloatTy(*ctx);
}

::llvm::Type *getJDoubleTy() {
  return Type::getDoubleTy(*ctx);
}

::llvm::PointerType *getJObjectTy() {
  return java_object_type_;
}

::llvm::PointerType *getJMethodTy() {
  return java_method_type_;
}

int getBitWidth(::llvm::Type *t) {
  if (t->isFloatTy() || t->isDoubleTy() || t->isIntegerTy(64))
    return 8;

  return 4;
}

/*
 * Converts a single-character primitive type into human-readable form.
 */
const char *primitiveTypeLabel(char typeChar) {
  switch (typeChar) {
  case 'B':
    return "byte";
  case 'C':
    return "char";
  case 'D':
    return "double";
  case 'F':
    return "float";
  case 'I':
    return "int";
  case 'J':
    return "long";
  case 'S':
    return "short";
  case 'V':
    return "void";
  case 'Z':
    return "boolean";
  default:
    return "UNKNOWN";
  } // switch
}

/*
 * Converts a type descriptor to human-readable "dotted" form.  For
 * example, "Ljava/lang/String;" becomes "java.lang.String", and
 * "[I" becomes "int[]".
 */
std::unique_ptr<char[]> descriptorToDot(const char *str) {
  int targetLen = strlen(str);
  int offset = 0;

  // Strip leading [s; will be added to end.
  while (targetLen > 1 && str[offset] == '[') {
    offset++;
    targetLen--;
  } // while

  const int arrayDepth = offset;

  if (targetLen == 1) {
    // Primitive type.
    str = primitiveTypeLabel(str[offset]);
    offset = 0;
    targetLen = strlen(str);
  } else {
    // Account for leading 'L' and trailing ';'.
    if (targetLen >= 2 && str[offset] == 'L' &&
        str[offset + targetLen - 1] == ';') {
      targetLen -= 2;
      offset++;
    }
  }

  // Copy class name over.
  std::unique_ptr<char[]> newStr(new char[targetLen + arrayDepth * 2 + 1]);
  int i = 0;
  for (; i < targetLen; i++) {
    const char ch = str[offset + i];
    newStr[i] = (ch == '/') ? '.' : ch;
  } // for

  // Add the appropriate number of brackets for arrays.
  for (int j = 0; j < arrayDepth; j++) {
    newStr[i++] = '[';
    newStr[i++] = ']';
  } // for

  newStr[i] = '\0';
  return newStr;
}

/*
 * Gets 2 little-endian bytes.
 */
u2 get2LE(unsigned char const *pSrc) {
  return pSrc[0] | (pSrc[1] << 8);
}

::llvm::Type *getJType(JType jty) {
  switch (jty) {
  case kVoid:
    return getJVoidTy();

  case kBoolean:
    return getJBooleanTy();

  case kByte:
    return getJByteTy();

  case kChar:
    return getJCharTy();

  case kShort:
    return getJShortTy();

  case kInt:
    return getJIntTy();

  case kLong:
    return getJLongTy();

  case kFloat:
    return getJFloatTy();

  case kDouble:
    return getJDoubleTy();

  case kObject:
    return getJObjectTy();

  default:
    LOG(FATAL) << "Unknown java type: " << jty;
    return NULL;
  }
}

inline JType GetJTypeFromShorty(char shorty_jty) {
  switch (shorty_jty) {
  case 'V':
    return kVoid;

  case 'Z':
    return kBoolean;

  case 'B':
    return kByte;

  case 'C':
    return kChar;

  case 'S':
    return kShort;

  case 'I':
    return kInt;

  case 'J':
    return kLong;

  case 'F':
    return kFloat;

  case 'D':
    return kDouble;

  case 'L':
    return kObject;

  default:
    LOG(FATAL) << "Unknown Dalvik shorty descriptor: " << shorty_jty;
    return kVoid;
  }
}

::llvm::Type *getJType(char shorty_jty) {
  return getJType(GetJTypeFromShorty(shorty_jty));
}

char RemapShorty(char shorty_type) {
  /*
   * TODO: might want to revisit this.  Dalvik registers are 32-bits wide,
   * and longs/doubles are represented as a pair of registers.  When sub-word
   * arguments (and method results) are passed, they are extended to Dalvik
   * virtual register containers.  Because llvm is picky about type consistency,
   * we must either cast the "real" type to 32-bit container multiple Dalvik
   * register types, or always use the expanded values.
   * Here, we're doing the latter.  We map the shorty signature to container
   * types (which is valid so long as we always do a real expansion of passed
   * arguments and field loads).
   */
  switch (shorty_type) {
  case 'Z':
    shorty_type = 'I';
    break;
  case 'B':
    shorty_type = 'I';
    break;
  case 'S':
    shorty_type = 'I';
    break;
  case 'C':
    shorty_type = 'I';
    break;
  default:
    break;
  }
  return shorty_type;
}

void printValue(::llvm::Value *v) {
  std::string _str;
  if (debug) {
    ::llvm::raw_string_ostream rso(_str);
    v->print(rso);
    LOG(WARNING) << std::string(40, '=');
    LOG(WARNING) << _str;
    LOG(WARNING) << std::string(40, '=');
  }
}

void printInst(::llvm::Instruction *i) {
  printValue(i);
}

::llvm::FunctionType *GetFunctionType(const char *shorty, u4 access_flags) {
  // Get return type
  ::llvm::Type *ret_type = getJType(RemapShorty(shorty[0]));

  // Get argument type
  std::vector<::llvm::Type *> args_type;

  // Do we have  a "this"?
  if ((access_flags & kAccStatic) == 0) {
    args_type.push_back(getJObjectTy());
  }

  for (u4 i = 1; i < strlen(shorty); ++i) {
    args_type.push_back(getJType(RemapShorty(shorty[i])));
  }

  ret_type->print(outs());
  fprintf(stdout, "\n%s %d\n", shorty, args_type.size());

  for (auto it = args_type.begin(); it != args_type.end(); it++) {
    (*it)->print(outs());
  }
  fprintf(stdout, "\n%s\n", shorty);

  return ::llvm::FunctionType::get(ret_type, args_type, false);
}

::llvm::Function *CreateFunctionDeclare(const DexFile *pDexFile,
                                        const dex::CodeItem *pCode,
                                        u4 idx, u4 flags,
                                        ::llvm::GlobalVariable::LinkageTypes type) {
  auto el = funcMap.find(idx);
  if (el != funcMap.end()) {
    return el->second;
  }

  ::llvm::Function *func;
  ::llvm::BasicBlock *bb;

  // TODO :: Create Function with parsed arguments
  {
    LOG(WARNING) << "============== Create Function Declare ===============";
    const char *shorty = pDexFile->GetMethodShorty(idx);
    ::llvm::FunctionType *func_type = GetFunctionType(shorty, flags);
    if (func_type == NULL) {
      return nullptr;
    }
    std::string symbol = pDexFile->PrettyMethod(idx);
    func = ::llvm::Function::Create(func_type, type, symbol, gMod);

    // TODO :: remove below codes after completed
    LOG(WARNING) << "============== Create Function Declare End =============";
  }

  return func;
}

::llvm::Function *CreateFunction(const DexFile *pDexFile,
                                 const dex::CodeItem *pCode,
                                 u4 idx, u4 flags,
                                 ::llvm::GlobalVariable::LinkageTypes type) {

  // invoke instruction needs which function will be called.
  // therefore, dummy function created before handle method.
  auto el = funcMap.find(idx);
  if (el != funcMap.end()) {
    return el->second;
  }

  CodeItemDebugInfoAccessor accessor(*pDexFile, pCode, idx);

  // TODO: much of this info available elsewhere.  Go to the original source?
  u2 num_dalvik_registers; // method->registers_size.
  u2 num_ins;
  u2 num_outs;
  u2 num_args;

  num_ins = accessor.InsSize();
  num_dalvik_registers = accessor.RegistersSize();
  num_args = num_dalvik_registers - num_ins;
  num_outs = accessor.OutsSize();

  vector<const char *> arg_descriptors;
  local_in_reg.clear();
  local_in_reg.resize(num_dalvik_registers);
  // TODO :: parse arguments type and reg
  LOG(WARNING) << "============== parsing arguments =============";
  // TODO : add this pointer if not static metohd

  const char *declaring_class_descriptor = pDexFile->GetMethodDeclaringClassDescriptor(pDexFile->GetMethodId(idx));

  // TODO :: remain arguments add
  DexFileParameterIterator it(*pDexFile, pDexFile->GetMethodPrototype(pDexFile->GetMethodId(idx)));

  // TODO :: remove below after code completed.
  LOG(WARNING) << "[DEBUG]";

  // TODO :: Create Function with parsed arguments
  LOG(WARNING) << "============== Create Function ===============";
  const char *shorty = pDexFile->GetMethodShorty(idx);
  ::llvm::FunctionType *func_type = GetFunctionType(shorty, flags);
  if (func_type == NULL) {
    return nullptr;
  }
  func_type->print(outs());

  std::string symbol = pDexFile->PrettyMethod(idx);
  func_ = ::llvm::Function::Create(func_type, type, symbol, gMod);
  func_->print(outs());

  LOG(WARNING) << "==============";

  int paramNum = func_type->getNumParams();
  int argStart = 0;
  int argPos = num_dalvik_registers;
  bool is_static = (flags & kAccStatic) != 0;
  bool is_constructor = (flags & kAccConstructor) != 0;

  // first of arguments always this if not the static method.
  if (!(is_static)) {
    argStart += 1;
  }

  int i = 0;
  while (--paramNum >= argStart) {
    LOG(WARNING) << "========2======"
                 << " param : " << paramNum;
    auto t = func_type->getParamType(paramNum);
    t->print(outs());

    char *arg_name = (char *)malloc(7); // register cannot over 65535
    const char *descriptor = declaring_class_descriptor;

    // treat register as 4byte variable. 
    // therefore, wide variable need 2 register. 
    argPos = argPos - getBitWidth(t) / 4;

    LocalInfo *li = &local_in_reg[argPos];
    sprintf(arg_name, "v%d", argPos);
    li->name_ = arg_name;
    li->descriptor_ = descriptor;
    li->signature_ = nullptr;
    li->start_address_ = 0;
    li->reg_ = argPos;
    li->is_live_ = true;
    li->val = func_->getArg(paramNum);
    li->val->setName(arg_name);
  }

  if (argStart > 0) {
    const char *descriptor = declaring_class_descriptor;
    LocalInfo *li = &local_in_reg[--argPos];
    li->name_ = "this";
    li->descriptor_ = descriptor;
    li->signature_ = nullptr;
    li->start_address_ = 0;
    li->reg_ = argPos;
    li->is_live_ = true;
    li->val = func_->getArg(paramNum);
    li->val->setName(li->name_);
  }

  if (debug) {
    LOG(WARNING) << "[DEBUG]";
    func_->print(outs());
    i = 0;
    for (vector<LocalInfo>::iterator it = local_in_reg.begin(); it != local_in_reg.end(); it++) {
      LOG(WARNING) << "[" << i++ << "] " << it->descriptor_ << " " << it->name_ << " " << it->reg_;
    }
  }

  LOG(WARNING) << "============== Create Function End =============";

  bb_ = ::llvm::BasicBlock::Create(*ctx, "bb", func_);
  Builder->SetInsertPoint(bb_);

  // TODO :: allocating local registers and set the value from argument
  {
    LOG(WARNING) << "============== Allocating Local Var =============";
    int pos = argPos;
    for (int i = 0; pos < num_dalvik_registers; pos++, i++) {
      LocalInfo *li = &local_in_reg[pos];
      if (li->val) {
        ::llvm::Value *localReg = new ::llvm::AllocaInst(li->val->getType(), 0, "", bb_);
        new ::llvm::StoreInst(li->val, localReg, bb_);
        li->val = new ::llvm::LoadInst(li->val->getType(), localReg, "", bb_);
        li->val->print(outs());
      }
    }

    LOG(WARNING) << "============== Allocating Local Var Ends =============";
  }

  return func_;
}

void keepMethods(ClassAccessor accessor) {
}
void init(ClassAccessor accessor) {
  keepMethods(accessor);

  ctx = new ::llvm::LLVMContext();
  gMod = new ::llvm::Module("test", *ctx);
  ::llvm::StructType::create(*ctx, "JavaObject");

  StructType *StructTy_JavaObject = StructType::create(gMod->getContext(), "JavaObject");
  std::vector<::llvm::Type *> StructTy_JavaObject_fields;
  if (StructTy_JavaObject->isOpaque()) {
    StructTy_JavaObject->setBody(StructTy_JavaObject_fields, /*isPacked=*/false);
  }

  java_object_type_ = StructTy_JavaObject->getPointerTo();
  java_method_type_ = java_object_type_;
}

void handleSkeleton(const DexFile *pDexFile,
                    const dex::CodeItem *pCode,
                    u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                    const Instruction *pDecInsn) {
  LOG(WARNING) << "===================================[SKELETON]=====================================";
  LOG(WARNING) << "===================================[SKELETON]=====================================";
}

void handleReturnVoid() {
  Builder->CreateRetVoid();
}

void handleConst(const DexFile *pDexFile,
                 const dex::CodeItem *pCode,
                 u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                 const Instruction *pDecInsn) {
  s8 value;
  s2 vreg;
  ::llvm::Type *constType;

  vreg = pDecInsn->VRegA();
  value = pDecInsn->VRegB();

  switch (pDecInsn->Opcode()) {
  case Instruction::CONST:
    constType = ::llvm::Type::getInt32Ty(*ctx);
    break;
  case Instruction::CONST_4:
    constType = ::llvm::Type::getInt8Ty(*ctx);
    break;
  case Instruction::CONST_16:
    constType = ::llvm::Type::getInt16Ty(*ctx);
    break;
  case Instruction::CONST_HIGH16:
    constType = ::llvm::Type::getInt32Ty(*ctx);
    value = value << 16;
    break;
  case Instruction::CONST_WIDE:
  case Instruction::CONST_WIDE_16:
  case Instruction::CONST_WIDE_32:
    constType = ::llvm::Type::getInt64Ty(*ctx);
    break;
  }

  ::llvm::Value *num = ::llvm::ConstantInt::get(constType, value, true);
  auto *alloc = new ::llvm::AllocaInst(constType, 0, "", bb_);
  auto *store = new ::llvm::StoreInst(num, alloc, bb_);
  auto *load = new ::llvm::LoadInst(constType, alloc, "", bb_);

  printInst(load);
  local_in_reg[vreg].val = load;
}

void handleInvokeDirect(const DexFile *pDexFile,
                        const dex::CodeItem *pCode,
                        u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                        const Instruction *pDecInsn) {
  u4 targetIdx = pDecInsn->VRegB_35c();
  u4 arg[5];
  pDecInsn->GetVarArgs(arg);

  fprintf(stdout, "===========================[TARGET:%d]==\n", targetIdx);
  const dex::MethodId &pMethodId = pDexFile->GetMethodId(targetIdx);
  ::llvm::Function *func = CreateFunctionDeclare(pDexFile, pCode, targetIdx, 0x00001, external);
  func->print(outs());
  const char *shorty = pDexFile->GetMethodShorty(targetIdx);
  ::llvm::FunctionType *func_type = GetFunctionType(shorty, 0x00001);

  std::vector<::llvm::Value *> args;
  for (size_t i = 0, count = pDecInsn->VRegA_35c(); i < count; ++i) {
    u4 v_reg = arg[i];
    LocalInfo *info = &local_in_reg[v_reg];
    auto fnTy = func_type->getParamType(i);
    auto regTy = info->val->getType();

    LOG(WARNING) << "HANDLE INVOKE : ";
    info->val->print(outs());
    // CHECK(fnTy == regTy);
    args.push_back(info->val);
  }

  ::llvm::CallInst *call = Builder->CreateCall(func_type, func, args);
  fprintf(stdout, "\n=============================\n");
  func_->print(outs());
  fprintf(stdout, "\n=============================\n");
}

void handleInvokeSuper(const DexFile *pDexFile,
                       const dex::CodeItem *pCode,
                       u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                       const Instruction *pDecInsn) {
  u4 targetIdx = pDecInsn->VRegB_35c();
  u4 arg[5];
  pDecInsn->GetVarArgs(arg);

  LOG(WARNING) << "===================================[InvokeSuper]=====================================";
  const dex::MethodId &pMethodId = pDexFile->GetMethodId(targetIdx);
  ::llvm::Function *func = CreateFunctionDeclare(pDexFile, pCode, targetIdx, 0x00001, external);
  const char *shorty = pDexFile->GetMethodShorty(targetIdx);
  ::llvm::FunctionType *func_type = GetFunctionType(shorty, 0x00001);

  std::vector<::llvm::Value *> args;
  for (size_t i = 0, count = pDecInsn->VRegA_35c(); i < count; ++i) {
    u4 v_reg = arg[i];
    LocalInfo *info = &local_in_reg[v_reg];
    auto fnTy = func_type->getParamType(i);
    auto regTy = info->val->getType();

    LOG(WARNING) << "HANDLE INVOKE : ";
    info->val->print(outs());
    // CHECK(fnTy == regTy);
    args.push_back(info->val);
  }

  ::llvm::CallInst *call = Builder->CreateCall(func_type, func, args);
  func_->print(outs());
  LOG(WARNING) << "===================================[InvokeSuper]=====================================";
}

void handleInvokeVirtual(const DexFile *pDexFile,
                         const dex::CodeItem *pCode,
                         u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                         const Instruction *pDecInsn) {
  u4 targetIdx = pDecInsn->VRegB_35c();
  u4 arg[5];
  pDecInsn->GetVarArgs(arg);

  LOG(WARNING) << "=======[InvokeVirtual : " << targetIdx << "]========";
  LOG(WARNING) << "=======" << pDecInsn->DumpString(pDexFile) << "\t======\n";
  const dex::MethodId &pMethodId = pDexFile->GetMethodId(targetIdx);
  ::llvm::Function *func = CreateFunctionDeclare(pDexFile, pCode, targetIdx, 0x00001, external);
  const char *shorty = pDexFile->GetMethodShorty(targetIdx);
  ::llvm::FunctionType *func_type = GetFunctionType(shorty, 0x00001);

  std::vector<::llvm::Value *> args;
  for (size_t i = 0, count = pDecInsn->VRegA_35c(); i < count; ++i) {
    u4 v_reg = arg[i];
    LocalInfo *info = &local_in_reg[v_reg];
    auto fnTy = func_type->getParamType(i);
    auto regTy = info->val->getType();

    LOG(WARNING) << "HANDLE INVOKE : ";
    info->val->print(outs());
    // CHECK(fnTy == regTy);
    args.push_back(info->val);
  }

  ::llvm::CallInst *call = Builder->CreateCall(func_type, func, args);
  func_->print(outs());
  LOG(WARNING) << "======[InvokeVirtual]======";
}

void handleInvoke(const DexFile *pDexFile,
                  const dex::CodeItem *pCode,
                  u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                  const Instruction *pDecInsn) {
  u4 targetIdx = pDecInsn->VRegB_35c();
  u4 arg[5];
  pDecInsn->GetVarArgs(arg);

  LOG(WARNING) << "=========================================================";
  const dex::MethodId &pMethodId = pDexFile->GetMethodId(targetIdx);
  ::llvm::Function *func = CreateFunctionDeclare(pDexFile, pCode, targetIdx, 0x00001, external);
  const char *shorty = pDexFile->GetMethodShorty(targetIdx);
  ::llvm::FunctionType *func_type = GetFunctionType(shorty, 0x00001);

  std::vector<::llvm::Value *> args;
  for (size_t i = 0, count = pDecInsn->VRegA_35c(); i < count; ++i) {
    u4 v_reg = arg[i];
    LocalInfo *info = &local_in_reg[v_reg];
    if (nullptr == info->val)
      break;

    auto fnTy = func_type->getParamType(i);
    auto regTy = info->val->getType();
    args.push_back(info->val);
  }

  ::llvm::CallInst *call = Builder->CreateCall(func_type, func, args);
  LOG(WARNING) << "=========================================================";
}

void handleMove(const DexFile *pDexFile,
                const dex::CodeItem *pCode,
                u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                const Instruction *pDecInsn) {
  LOG(WARNING) << "===================================[MOVE]=====================================";

  switch (pDecInsn->Opcode()) {
  case Instruction::MOVE:
  case Instruction::MOVE_FROM16:
  case Instruction::MOVE_16:
  case Instruction::MOVE_WIDE:
  case Instruction::MOVE_WIDE_FROM16:
  case Instruction::MOVE_WIDE_16:
  case Instruction::MOVE_OBJECT:
  case Instruction::MOVE_OBJECT_16:
  case Instruction::MOVE_OBJECT_FROM16:
    u2 vA = pDecInsn->VRegA();
    u2 vB = pDecInsn->VRegB();
    ::llvm::Value *val_vA = local_in_reg[vA].val;
    ::llvm::Value *val_vB = local_in_reg[vB].val;
    if (nullptr == val_vA || nullptr == val_vB)
      break;

    new ::llvm::StoreInst(val_vB, val_vA, bb_);
    break;
  }
}

void handleMoveResult(const DexFile *pDexFile,
                      const dex::CodeItem *pCode,
                      u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                      const Instruction *pDecInsn) {
  // TODO : must be invoke instruction previous move-result instruction
  // so, following codes are no need after done parsing all instruction
  if (bb_->getInstList().size() == 0)
    return;

  ::llvm::Instruction *i = dyn_cast<::llvm::Instruction>(--bb_->end());
  CallInst *cinst = dyn_cast<CallInst>(i);

  LOG(WARNING) << "=============\n";
  if (!cinst) {
    LOG(WARNING) << "unexpected previous instruction!!";
    return;
  }
  ::llvm::Type *fnRetType = cinst->getCalledFunction()->getReturnType();
  cinst->print(outs());
  outs() << "=============\n";
  fnRetType->print(outs());
  outs() << "=============\n";

  u1 vA = pDecInsn->VRegA();
  LocalInfo *li = &local_in_reg[vA];
  if (li->val == nullptr) {
  outs() << "=============\n";

    li->val = new ::llvm::AllocaInst(fnRetType, 0, "", bb_);
  }

  //new ::llvm::StoreInst(li->val, cinst, bb_);
}

void handleReturn(const DexFile *pDexFile,
                  const dex::CodeItem *pCode,
                  u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                  const Instruction *pDecInsn) {
  switch (pDecInsn->Opcode()) {
  case Instruction::RETURN_OBJECT:
  case Instruction::RETURN_WIDE:
  case Instruction::RETURN: {
    u1 vA = pDecInsn->VRegA();
    ::llvm::Value *val = local_in_reg[vA].val;
    Builder->CreateRet(val);
  } break;
  case Instruction::RETURN_VOID:
    Builder->CreateRetVoid();
    break;
  }
}

::llvm::Value *allocPtrReg(u2 idx) {
  LocalInfo *reg = &local_in_reg[idx];

  if (!reg->val) {
  }

  return reg->val;
}
::llvm::Value *allocLongReg(u2 idx) {
  LocalInfo *reg = &local_in_reg[idx];

  if (!reg->val) {
  }

  return reg->val;
}

::llvm::Value *allocIntReg(u2 idx) {
  LocalInfo *reg = &local_in_reg[idx];

  if (!reg->val) {
    // reg->val = new ::llvm::Value(::llvm::Type::getInt32Ty(*ctx));
  }

  return reg->val;
}

void handleArithOp(const DexFile *pDexFile,
                   const dex::CodeItem *pCode,
                   u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                   const Instruction *pDecInsn) {
  u2 vA, vB, vC;
  ::llvm::Value *op1, *op2, *res;

  auto code = pDecInsn->Opcode();
  ::llvm::Type *ty;
  if (code >= Instruction::ADD_INT && code <= Instruction::USHR_INT) {
    ty = Type::getInt32Ty(*ctx);
  }
  else if (code >= Instruction::ADD_LONG && code <= Instruction::USHR_LONG) {
    ty = Type::getInt64Ty(*ctx); 
  }
  else if (code >= Instruction::ADD_FLOAT && code <= Instruction::REM_FLOAT) {
    ty = Type::getFloatTy(*ctx);
  }
  else if (code >= Instruction::ADD_DOUBLE && code <= Instruction::REM_DOUBLE) {
    ty = Type::getDoubleTy(*ctx);
  }

  vA = pDecInsn->VRegA();
  vB = pDecInsn->VRegB();
  if (Instruction::FormatOf(code) == Instruction::k23x) {
    vC = pDecInsn->VRegC();
    op1 = local_in_reg[vB].val;
    op2 = local_in_reg[vC].val;

  } else {
    op1 = local_in_reg[vA].val;
    op2 = local_in_reg[vB].val;
  }

  switch (Instruction::EFlagsOf(code)) {
    case Instruction::kAdd:
    res = Builder->CreateAdd(op1, op2);
    break;
    case Instruction::kSubtract:
    res = Builder->CreateSub(op1, op2);
    break;
    case Instruction::kMultiply:
    res = Builder->CreateMul(op1, op2);
    break;
    case Instruction::kDivide:
    res = Builder->CreateSDiv(op1, op2);
    break;
    case Instruction::kRemainder:
    res = Builder->CreateSRem(op1, op2);
    break;
    case Instruction::kAnd:
    res = Builder->CreateAnd(op1, op2);
    break;
    case Instruction::kOr:
    res = Builder->CreateOr(op1, op2);
    break;
    case Instruction::kXor:
    res = Builder->CreateXor(op1, op2);
    break;
    case Instruction::kShl:
    break;
    case Instruction::kShr:
    break;
    case Instruction::kUshr:
    break;
    default:
      LOG(FATAL) << "unexpected instruction ";
  }

  if (nullptr == res)
    goto err;

  local_in_reg[vA].val = res;
  return;

err:
  LOG(FATAL) << "INSTRUCTION HANDLER MISMATCHED ";
  exit(EXIT_FAILURE);
}

void handleIget(const DexFile *pDexFile,
                const dex::CodeItem *pCode,
                u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                const Instruction *pDecInsn) {
  u2 vA, vB, vC;
  vA = pDecInsn->VRegA();
  vB = pDecInsn->VRegB();
  vC = pDecInsn->VRegC(); // field_idx

  // TODO : remove after complete

  // Get Field Type
  const dex::FieldId& field_id = pDexFile->GetFieldId(vC);
  const char *ty = pDexFile->GetFieldTypeDescriptor(field_id);
  LOG(WARNING) << ty;
  //::llvm::Type *lty = getJType(ty);

  LocalInfo *op1 = &local_in_reg[vA];
  LocalInfo *op2 = &local_in_reg[vB];

  Builder->CreateStore(op1->val, op2->val);

}

void dumpInstructionAsIR(const DexFile *pDexFile,
                         const dex::CodeItem *pCode,
                         u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                         const Instruction *pDecInsn) {
  LOG(WARNING) << "======================================================";
  LOG(WARNING) << "[iNSTRUCTION]\t " << pDecInsn->DumpString(pDexFile);

  switch (pDecInsn->Opcode()) {
  case Instruction::MOVE:
  case Instruction::MOVE_FROM16:
  case Instruction::MOVE_16:
  case Instruction::MOVE_WIDE:
  case Instruction::MOVE_WIDE_FROM16:
  case Instruction::MOVE_WIDE_16:
  case Instruction::MOVE_OBJECT:
  case Instruction::MOVE_OBJECT_16:
  case Instruction::MOVE_OBJECT_FROM16:
    handleMove(pDexFile, pCode, codeOffset, insnIdx, insnWidth, flags, pDecInsn);
    break;

  case Instruction::MOVE_RESULT:
  case Instruction::MOVE_RESULT_WIDE:
  case Instruction::MOVE_RESULT_OBJECT:
    handleMoveResult(pDexFile, pCode, codeOffset, insnIdx, insnWidth, flags, pDecInsn);
    break;
  case Instruction::RETURN_OBJECT:
  case Instruction::RETURN_WIDE:
  case Instruction::RETURN:
  case Instruction::RETURN_VOID:
    handleReturn(pDexFile, pCode, codeOffset, insnIdx, insnWidth, flags, pDecInsn);
    break;
  case Instruction::INVOKE_DIRECT:
  case Instruction::INVOKE_SUPER:
  case Instruction::INVOKE_VIRTUAL:
  case Instruction::INVOKE_STATIC:
  case Instruction::INVOKE_INTERFACE:
    handleInvoke(pDexFile, pCode, codeOffset, insnIdx, insnWidth, flags, pDecInsn);
    break;
  case Instruction::CONST:
  case Instruction::CONST_4:
  case Instruction::CONST_16:
  case Instruction::CONST_HIGH16:
  case Instruction::CONST_WIDE:
  case Instruction::CONST_WIDE_16:
  case Instruction::CONST_WIDE_32:
    handleConst(pDexFile, pCode, codeOffset, insnIdx, insnWidth, flags, pDecInsn);
    break;

  case Instruction::ADD_LONG:
  case Instruction::ADD_LONG_2ADDR:
  case Instruction::ADD_INT:
  case Instruction::ADD_INT_2ADDR:
  case Instruction::SUB_LONG:
  case Instruction::SUB_LONG_2ADDR:
  case Instruction::SUB_INT:
  case Instruction::SUB_INT_2ADDR:
  case Instruction::MUL_LONG:
  case Instruction::MUL_LONG_2ADDR:
  case Instruction::MUL_INT:
  case Instruction::MUL_INT_2ADDR:
  case Instruction::DIV_LONG:
  case Instruction::DIV_LONG_2ADDR:
  case Instruction::DIV_INT:
  case Instruction::DIV_INT_2ADDR:
  case Instruction::REM_LONG:
  case Instruction::REM_LONG_2ADDR:
  case Instruction::REM_INT:
  case Instruction::REM_INT_2ADDR:
  case Instruction::AND_LONG:
  case Instruction::AND_LONG_2ADDR:
  case Instruction::AND_INT:
  case Instruction::AND_INT_2ADDR:
  case Instruction::OR_LONG:
  case Instruction::OR_LONG_2ADDR:
  case Instruction::OR_INT:
  case Instruction::OR_INT_2ADDR:
  case Instruction::XOR_LONG:
  case Instruction::XOR_LONG_2ADDR:
  case Instruction::XOR_INT:
  case Instruction::XOR_INT_2ADDR:
    handleArithOp(pDexFile, pCode, codeOffset, insnIdx, insnWidth, flags, pDecInsn);
    break;

  case Instruction::IGET:
  case Instruction::IGET_WIDE:
  case Instruction::IGET_OBJECT:
  case Instruction::IGET_BOOLEAN:
  case Instruction::IGET_BYTE:
  case Instruction::IGET_CHAR:
  case Instruction::IGET_SHORT:
    handleIget(pDexFile, pCode, codeOffset, insnIdx, insnWidth, flags, pDecInsn);
    break;

  default:
    LOG(WARNING) << "[UNSUPPORTED iNSTRUCTION] ";
    break;
  } // switch
}

void dumpBytecodesAsIR(const DexFile *pDexFile, u4 idx, u4 flags,
                       const dex::CodeItem *pCode, u4 codeOffset) {
  fprintf(stdout, "in %s\n", __FUNCTION__);

  Builder = new ::llvm::IRBuilder<>(*ctx);

  const dex::MethodId &pMethodId = pDexFile->GetMethodId(idx);
  const char *name = pDexFile->StringDataByIdx(pMethodId.name_idx_);
  const Signature signature = pDexFile->GetMethodSignature(pMethodId);
  const char *backDescriptor = pDexFile->StringByTypeIdx(pMethodId.class_idx_);
  const char *shorty = pDexFile->GetMethodShorty(idx);

  LOG(WARNING) << "======================================================";
  LOG(WARNING) << "[METHOD]\t " << name;
  // Generate header.
  std::unique_ptr<char[]> dot(descriptorToDot(backDescriptor));
  fprintf(stdout, "%06x:  |[%06x] %s [name] %s: [signature] %s [shorty] %s\n",
          codeOffset, codeOffset, dot.get(), name, signature.ToString().c_str(), shorty);

  // Iterate over all instructions.
  CodeItemDataAccessor accessor(*pDexFile, pCode);
  const u4 maxPc = accessor.InsnsSizeInCodeUnits();

  CreateFunction(pDexFile, pCode, idx, flags, internal);
  func_->print(::llvm::outs());

  for (const DexInstructionPcPair &pair : accessor) {
    const u4 dexPc = pair.DexPc();
    if (dexPc >= maxPc) {
      LOG(WARNING) << "GLITCH: run-away instruction at idx=0x" << std::hex << dexPc;
      break;
    }
    const Instruction *instruction = &pair.Inst();
    const u4 insnWidth = instruction->SizeInCodeUnits();
    if (insnWidth == 0) {
      LOG(WARNING) << "GLITCH: zero-width instruction at idx=0x" << std::hex << dexPc;
      break;
    }
    dumpInstructionAsIR(pDexFile, pCode, codeOffset, dexPc, insnWidth, flags, instruction);
  } // for

  fprintf(stdout, "\n=========[SAVING]==============\n");
  std::string str;
  ::llvm::raw_string_ostream ss(str);
  func_->print(ss);
  fprintf(gOutFile, "%s", str.c_str());
  fprintf(stdout, "\n=========[SAVING]=============\n");
}

} // namespace llvm
} // namespace art
