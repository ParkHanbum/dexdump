#include "ir_builder.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/Cloning.h"



using namespace llvm;
using namespace std;

namespace art {
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

FILE* gOutFile = stdout;

::llvm::PointerType *java_object_type_;
::llvm::PointerType *java_method_type_;
::llvm::PointerType *java_thread_type_;

::llvm::LLVMContext* ctx;
::llvm::Module* gMod;
::llvm::Function* func_;
::llvm::BasicBlock *bb_;
std::string symbol_;
::llvm::IRBuilder<> *Builder;

std::unordered_map<u4, ::llvm::Function *> funcMap;
std::unordered_map<u4, const ClassAccessor::Method&> methodMap;

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


/*
 * Converts a single-character primitive type into human-readable form.
 */
const char* primitiveTypeLabel(char typeChar) {
  switch (typeChar) {
    case 'B': return "byte";
    case 'C': return "char";
    case 'D': return "double";
    case 'F': return "float";
    case 'I': return "int";
    case 'J': return "long";
    case 'S': return "short";
    case 'V': return "void";
    case 'Z': return "boolean";
    default:  return "UNKNOWN";
  }  // switch
}


/*
 * Converts a type descriptor to human-readable "dotted" form.  For
 * example, "Ljava/lang/String;" becomes "java.lang.String", and
 * "[I" becomes "int[]".
 */
std::unique_ptr<char[]> descriptorToDot(const char* str) {
  int targetLen = strlen(str);
  int offset = 0;

  // Strip leading [s; will be added to end.
  while (targetLen > 1 && str[offset] == '[') {
    offset++;
    targetLen--;
  }  // while

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
  }  // for

  // Add the appropriate number of brackets for arrays.
  for (int j = 0; j < arrayDepth; j++) {
    newStr[i++] = '[';
    newStr[i++] = ']';
  }  // for

  newStr[i] = '\0';
  return newStr;
}

/*
 * Gets 2 little-endian bytes.
 */
u2 get2LE(unsigned char const* pSrc) {
  return pSrc[0] | (pSrc[1] << 8);
}

::llvm::Type* getJType(JType jty) {
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
    case 'Z' : shorty_type = 'I'; break;
    case 'B' : shorty_type = 'I'; break;
    case 'S' : shorty_type = 'I'; break;
    case 'C' : shorty_type = 'I'; break;
    default: break;
  }
  return shorty_type;
}



::llvm::FunctionType* GetFunctionType(const char *shorty, u4 access_flags) {
  // Get return type
  ::llvm::Type* ret_type = getJType(RemapShorty(shorty[0]));

  // Get argument type
  std::vector< ::llvm::Type*> args_type;

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
  u2 num_dalvik_registers;        // method->registers_size.
  u2 num_ins;
  u2 num_outs;
  u2 num_args;

  num_ins = accessor.InsSize();
  num_dalvik_registers = accessor.RegistersSize();
  num_args = num_dalvik_registers - num_ins;
  num_outs = accessor.OutsSize();

  vector<const char*> arg_descriptors;
  local_in_reg.resize(num_dalvik_registers);
  bool is_static = (flags & kAccStatic) != 0;

  // TODO :: parse arguments type and reg
  {
    LOG(WARNING) << "============== parsing arguments =============";
    // TODO : add this pointer if not static metohd
    u2 arg_reg = num_args;
    const char *declaring_class_descriptor = pDexFile->GetMethodDeclaringClassDescriptor(pDexFile->GetMethodId(idx));
    if (!is_static) {
      const char *descriptor = declaring_class_descriptor;
      local_in_reg[arg_reg].name_ = "this";
      local_in_reg[arg_reg].descriptor_ = descriptor;
      local_in_reg[arg_reg].signature_ = nullptr;
      local_in_reg[arg_reg].start_address_ = 0;
      local_in_reg[arg_reg].reg_ = arg_reg;
      local_in_reg[arg_reg].is_live_ = true;
      arg_reg++;
    }

    // TODO :: remain arguments add
    DexFileParameterIterator it(*pDexFile, pDexFile->GetMethodPrototype(pDexFile->GetMethodId(idx)));
    for (; it.HasNext(); it.Next()) {
      char *arg_name = (char *)malloc(7); // register cannot over 65535
      const char *descriptor = declaring_class_descriptor;
      sprintf(arg_name, "v%d", arg_reg);
      local_in_reg[arg_reg].name_ = arg_name;
      local_in_reg[arg_reg].descriptor_ = descriptor;
      local_in_reg[arg_reg].signature_ = nullptr;
      local_in_reg[arg_reg].start_address_ = 0;
      local_in_reg[arg_reg].reg_ = arg_reg;
      local_in_reg[arg_reg].is_live_ = true;
      arg_reg++;
    }

    // TODO :: remove below after code completed.
    LOG(WARNING) << "[DEBUG]";
    for (vector<LocalInfo>::iterator it = local_in_reg.begin(); it != local_in_reg.end(); it++) {
      LOG(WARNING) << it->descriptor_ << it->name_ << it->reg_;
    }

    LOG(WARNING) << "============== parsing ENDs =============";
  }

  // TODO :: Create Function with parsed arguments
  {
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
    u2 arg_reg = num_args;
    for (int i = 0;arg_reg < num_dalvik_registers;arg_reg++, i++) {
      LocalInfo *li = &local_in_reg[arg_reg];
      li->val = func_->getArg(i);
      li->val->setName(li->name_);
    }
    LOG(WARNING) << "==============";

    // TODO :: remove below codes after completed
    LOG(WARNING) << "[DEBUG]";
    func_->print(outs());
    LOG(WARNING) << "============== Create Function End =============";
  }

  bb_ = ::llvm::BasicBlock::Create(*ctx, "bb", func_);
  Builder->SetInsertPoint(bb_);

  // TODO :: allocating local registers and set the value from argument
  {
    LOG(WARNING) << "============== Allocating Local Var =============";

    u2 arg_reg = num_args;
    for (int i = 0; arg_reg < num_dalvik_registers; arg_reg++, i++) {
      LocalInfo *info = &local_in_reg[arg_reg];
      ::llvm::Value* localReg = new ::llvm::AllocaInst(info->val->getType(), 0, "", bb_);
      new ::llvm::StoreInst(info->val, localReg, bb_);
      info->val = new ::llvm::LoadInst(info->val->getType(), localReg, "", bb_);
      info->val->print(outs());
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
  
  StructType* StructTy_JavaObject = StructType::create(gMod->getContext(), "JavaObject");
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

void handleInvokeDirect(const DexFile *pDexFile,
                         const dex::CodeItem *pCode,
                         u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                         const Instruction *pDecInsn)  {
  u4 targetIdx = pDecInsn->VRegB_35c();
  u4 arg[5];
  pDecInsn->GetVarArgs(arg);

  fprintf(stdout, "=============================\n");
  const dex::MethodId& pMethodId = pDexFile->GetMethodId(targetIdx);
  const dex::ClassDef& ClassDef = pDexFile->GetClassDef(pMethodId.class_idx_.index_);
  ClassAccessor accessor(*pDexFile, ClassDef, /* parse_hiddenapi_class_data= */ true);
  const char* classDescriptor = pDexFile->StringByTypeIdx(ClassDef.class_idx_);
  fprintf(stdout, "  Class descriptor  : '%s'\t %s\n", classDescriptor, pDecInsn->DumpString(pDexFile).c_str());

  u4 i = 0u;
  const ClassAccessor::Method *target = nullptr;

  for (const ClassAccessor::Method& method : accessor.GetDirectMethods()) {
    const dex::MethodId &pMethodId = pDexFile->GetMethodId(method.GetIndex());
    const char *name = pDexFile->StringDataByIdx(pMethodId.name_idx_);
    fprintf(stdout, "%s\n", name);

    if (method.GetIndex() == targetIdx) {
      target = &method;
    }
    ++i;
  }
  if (target == nullptr) {
    i = 0u;
    for (const ClassAccessor::Method &method : accessor.GetVirtualMethods()) {
    const dex::MethodId &pMethodId = pDexFile->GetMethodId(method.GetIndex());
    const char *name = pDexFile->StringDataByIdx(pMethodId.name_idx_);
    fprintf(stdout, "%s\n", name);
    if (method.GetIndex() == targetIdx) {
      target = &method;
    }
    ++i;
    }
  }

  u4 targetFlags;
  // this method not exist in this current dex file, maybe it primitive type
  if (target == nullptr) {
    targetFlags = 0x00001; // TODO : make ensure assume
    ::llvm::Function *func = CreateFunction(pDexFile, pCode, targetIdx, targetFlags, external);
  } else {
    targetFlags = target->GetAccessFlags();
    ::llvm::Function *func = CreateFunction(pDexFile, pCode, targetIdx, targetFlags, internal);
  }

  const char *shorty = pDexFile->GetMethodShorty(targetIdx);
  ::llvm::FunctionType* func_type = GetFunctionType(shorty, targetFlags);


  std::vector< ::llvm::Value*> args;
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

  ::llvm::CallInst *call = Builder->CreateCall(func_type, func_, args);
  fprintf(stdout, "\n=============================\n");
  func_->print(outs());
  fprintf(stdout, "\n=============================\n");
}

void dumpInstructionAsIR(const DexFile *pDexFile,
                         const dex::CodeItem *pCode,
                         u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                         const Instruction *pDecInsn)  {
  switch(pDecInsn->Opcode()) {
    case Instruction::RETURN_VOID:
      handleReturnVoid();
      break;
    case Instruction::RETURN:
    case Instruction::RETURN_WIDE:
    case Instruction::RETURN_OBJECT:
      break;
    case Instruction::INVOKE_DIRECT:
      handleInvokeDirect(pDexFile, pCode, codeOffset, insnIdx, insnWidth, flags, pDecInsn);
      break;
    case Instruction::INVOKE_SUPER:
      break;
    case Instruction::CONST_HIGH16:
      break;

    default:
      fprintf(gOutFile, " ???");
      break;
  } // switch
}



void dumpBytecodesAsIR(const DexFile *pDexFile, u4 idx, u4 flags,
                       const dex::CodeItem *pCode, u4 codeOffset) {
  fprintf(stdout, "in %s\n", __FUNCTION__);

  Builder = new ::llvm::IRBuilder<>(*ctx);

  const dex::MethodId& pMethodId = pDexFile->GetMethodId(idx);
  const char* name = pDexFile->StringDataByIdx(pMethodId.name_idx_);
  const Signature signature = pDexFile->GetMethodSignature(pMethodId);
  const char* backDescriptor = pDexFile->StringByTypeIdx(pMethodId.class_idx_);
  const char* shorty = pDexFile->GetMethodShorty(idx);

  // Generate header.
  std::unique_ptr<char[]> dot(descriptorToDot(backDescriptor));
  fprintf(stdout, "%06x:  |[%06x] %s [name] %s: [signature] %s [shorty] %s\n",
          codeOffset, codeOffset, dot.get(), name, signature.ToString().c_str(), shorty);

  // Iterate over all instructions.
  CodeItemDataAccessor accessor(*pDexFile, pCode);
  const u4 maxPc = accessor.InsnsSizeInCodeUnits();

  CreateFunction(pDexFile, pCode, idx, flags, internal);
  func_->print(::llvm::outs());



  for (const DexInstructionPcPair& pair : accessor) {
    const u4 dexPc = pair.DexPc();
    if (dexPc >= maxPc) {
      LOG(WARNING) << "GLITCH: run-away instruction at idx=0x" << std::hex << dexPc;
      break;
    }
    const Instruction* instruction = &pair.Inst();
    const u4 insnWidth = instruction->SizeInCodeUnits();
    if (insnWidth == 0) {
      LOG(WARNING) << "GLITCH: zero-width instruction at idx=0x" << std::hex << dexPc;
      break;
    }
    dumpInstructionAsIR(pDexFile, pCode, codeOffset, dexPc, insnWidth, flags, instruction);
  } // for

  fprintf(stdout, "\n=============================\n");
  func_->print(outs());
  fprintf(stdout, "\n=============================\n");
}

} // namespace llvm
} // namespace art

