#ifndef LIBLLVM_IR_BUILDER_H_
#define LIBLLVM_IR_BUILDER_H_

#include <hash_map>
#include <iostream>
#include <unordered_map>

#include "android-base/file.h"
#include "android-base/logging.h"
#include "android-base/stringprintf.h"

#include "dex/class_accessor-inl.h"
#include "dex/code_item_accessors-inl.h"
#include "dex/dex_file-inl.h"
#include "dex/dex_file.h"
#include "dex/dex_file_exception_helpers.h"
#include "dex/dex_file_loader.h"
#include "dex/dex_file_types.h"
#include "dex/dex_instruction-inl.h"

/*
 * Data types that match the definitions in the VM specification.
 */
using u1 = uint8_t;
using u2 = uint16_t;
using u4 = uint32_t;
using u8 = uint64_t;
using s1 = int8_t;
using s2 = int16_t;
using s4 = int32_t;
using s8 = int64_t;

namespace art {
namespace llvm {

enum OpKind {
  kOpMov,
  kOpCmov,
  kOpMvn,
  kOpCmp,
  kOpLsl,
  kOpLsr,
  kOpAsr,
  kOpRor,
  kOpNot,
  kOpAnd,
  kOpOr,
  kOpXor,
  kOpNeg,
  kOpAdd,
  kOpAdc,
  kOpSub,
  kOpSbc,
  kOpRsub,
  kOpMul,
  kOpDiv,
  kOpRem,
  kOpBic,
  kOpCmn,
  kOpTst,
  kOpRev,
  kOpRevsh,
  kOpBkpt,
  kOpBlx,
  kOpPush,
  kOpPop,
  kOp2Char,
  kOp2Short,
  kOp2Byte,
  kOpCondBr,
  kOpUncondBr,
  kOpBx,
  kOpInvalid,
};

void init(ClassAccessor accessor);

void dumpInstructionAsIR(const DexFile *pDexFile,
                         const dex::CodeItem *pCode,
                         u4 codeOffset, u4 insnIdx, u4 insnWidth, u4 flags,
                         const Instruction *pDecInsn);

void dumpBytecodesAsIR(const DexFile *pDexFile, u4 idx, u4 flags,
                       const dex::CodeItem *pCode, u4 codeOffset);

} // namespace llvm
} // namespace art

#endif // LIBLLVM_IR_BUILDER_H_
