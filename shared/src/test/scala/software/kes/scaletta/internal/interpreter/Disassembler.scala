package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.common.BasicTypes

object Disassembler {
  def disassemble(userFunction: UserFunction): String = {
    disassemble(userFunction, None)
  }

  def disassemble(userFunction: UserFunction, constantPool: ConstantPool): String = {
    disassemble(userFunction, Some(constantPool))
  }

  private def disassemble(userFunction: UserFunction, constantPool: Option[ConstantPool]): String = {
    val sb = new StringBuilder
    var ip = 0
    val length = userFunction.instructions.length

    while (ip < length) {
      val address = ip
      val rawOpcode = userFunction.fetch(ip)
      val opcode = (rawOpcode >> 24) & 0xFF
      ip += 1

      sb.append(f"$address%04d: ")

      opcode match {
        case Opcodes.Nop =>
          sb.append("NOP")

        case Opcodes.PushConst =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          val value = (rawOpcode & 0xFFFF).toShort
          sb.append(s"PUSH_CONST ${BasicTypes.friendlyName(typeTag.toByte)} $value")
          if (typeTag == BasicTypes.Object) {
            appendConstant(sb, constantPool, typeTag.toByte, value & 0xFFFF)
          }

        case Opcodes.Push =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          val operand = userFunction.fetch(ip)
          ip += 1
          sb.append(s"PUSH ${BasicTypes.friendlyName(typeTag.toByte)} $operand")
          if (isConstantPoolType(typeTag.toByte)) {
            appendConstant(sb, constantPool, typeTag.toByte, operand)
          }

        case Opcodes.StoreConst =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          val varIndex = (rawOpcode >> 8) & 0xFF
          val value = rawOpcode & 0xFF
          sb.append(s"STORE_CONST ${BasicTypes.friendlyName(typeTag.toByte)} v$varIndex, $value")
          if (isConstantPoolType(typeTag.toByte)) {
            appendConstant(sb, constantPool, typeTag.toByte, value)
          }

        case Opcodes.Store =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          val varIndex = rawOpcode & 0xFFFF
          val operand = userFunction.fetch(ip)
          ip += 1
          sb.append(s"STORE ${BasicTypes.friendlyName(typeTag.toByte)} v$varIndex, $operand")
          if (isConstantPoolType(typeTag.toByte)) {
            appendConstant(sb, constantPool, typeTag.toByte, operand)
          }

        case Opcodes.StoreWide =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          val varIndex = userFunction.fetch(ip)
          ip += 1
          val operand = userFunction.fetch(ip)
          ip += 1
          sb.append(s"STORE_WIDE ${BasicTypes.friendlyName(typeTag.toByte)} v$varIndex, $operand")
          if (isConstantPoolType(typeTag.toByte)) {
            appendConstant(sb, constantPool, typeTag.toByte, operand)
          }

        case Opcodes.PushFromVar =>
          val varIndex = rawOpcode & 0xFFFF
          sb.append(s"PUSH_FROM_VAR v$varIndex")

        case Opcodes.PushFromVarWide =>
          val varIndex = userFunction.fetch(ip)
          ip += 1
          sb.append(s"PUSH_FROM_VAR_WIDE v$varIndex")

        case Opcodes.PopIntoVar =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          val varIndex = rawOpcode & 0xFFFF
          sb.append(s"POP_INTO_VAR ${BasicTypes.friendlyName(typeTag.toByte)} v$varIndex")

        case Opcodes.PopIntoVarWide =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          val varIndex = userFunction.fetch(ip)
          ip += 1
          sb.append(s"POP_INTO_VAR_WIDE ${BasicTypes.friendlyName(typeTag.toByte)} v$varIndex")

        case Opcodes.Branch =>
          val offset = getSigned24(rawOpcode)
          sb.append(s"BRANCH ${address + 1 + offset} ($offset)")

        case Opcodes.BranchIf =>
          val offset = getSigned24(rawOpcode)
          sb.append(s"BRANCH_IF ${address + 1 + offset} ($offset)")

        case Opcodes.BranchUnless =>
          val offset = getSigned24(rawOpcode)
          sb.append(s"BRANCH_UNLESS ${address + 1 + offset} ($offset)")

        case Opcodes.Dup =>
          sb.append("DUP")

        case Opcodes.Swap =>
          sb.append("SWAP")

        case Opcodes.Pop =>
          sb.append("POP")

        case Opcodes.CallNative =>
          val nativeId = rawOpcode & 0xFFFFFF
          sb.append(s"CALL_NATIVE $nativeId")

        case Opcodes.CallLocal =>
          val functionIndex = rawOpcode & 0xFFFFFF
          sb.append(s"CALL_LOCAL f$functionIndex")

        case Opcodes.TailCallLocal =>
          val functionIndex = rawOpcode & 0xFFFFFF
          sb.append(s"TAIL_CALL_LOCAL f$functionIndex")

        case Opcodes.Return =>
          sb.append("RETURN")

        case Opcodes.LogicalAnd =>
          val offset = getSigned24(rawOpcode)
          sb.append(s"LOGICAL_AND ${address + 1 + offset} ($offset)")

        case Opcodes.LogicalOr =>
          val offset = getSigned24(rawOpcode)
          sb.append(s"LOGICAL_OR ${address + 1 + offset} ($offset)")

        case Opcodes.Box =>
          sb.append("BOX")

        case Opcodes.Convert =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          sb.append(s"CONVERT ${BasicTypes.friendlyName(typeTag.toByte)}")

        case Opcodes.StringConcat =>
          val numArgs = rawOpcode & 0xFFFFFF
          sb.append(s"STRING_CONCAT $numArgs")

        case Opcodes.LazyInit =>
          val typeTag = (rawOpcode >> 16) & 0xFF
          val varIndex = userFunction.fetch(ip)
          ip += 1
          sb.append(s"LAZY_INIT ${BasicTypes.friendlyName(typeTag.toByte)} v$varIndex")

        case Opcodes.LazyEval =>
          val varIndex = userFunction.fetch(ip)
          ip += 1
          val evalFunctionIndex = userFunction.fetch(ip)
          ip += 1
          sb.append(s"LAZY_EVAL v$varIndex, f$evalFunctionIndex")

        case Opcodes.MakeClosure =>
          val functionIndex = rawOpcode & 0xFFFFFF
          val capturePlanIndex = userFunction.fetch(ip)
          ip += 1
          sb.append(s"MAKE_CLOSURE f$functionIndex, cp$capturePlanIndex")
          if (capturePlanIndex != 0) {
            appendConstant(sb, constantPool, BasicTypes.Object, capturePlanIndex)
          } else {
            sb.append(" (empty)")
          }

        case Opcodes.CallClosure =>
          sb.append("CALL_CLOSURE")

        case Opcodes.TailCallClosure =>
          sb.append("TAIL_CALL_CLOSURE")

        case _ =>
          sb.append(s"UNKNOWN_OPCODE $opcode")
      }
      sb.append("\n")
    }
    sb.toString()
  }

  private def getSigned24(rawOpcode: Int): Int = {
    val offset = rawOpcode & 0xFFFFFF
    if ((offset & 0x800000) != 0) offset | 0xFF000000 else offset
  }

  private def isConstantPoolType(typeTag: Byte): Boolean = {
    typeTag match {
      case BasicTypes.Long | BasicTypes.Double | BasicTypes.Float | BasicTypes.Object => true
      case _ => false
    }
  }

  private def appendConstant(sb: StringBuilder, constantPool: Option[ConstantPool], typeTag: Byte, index: Int): Unit = {
    constantPool.foreach { cp =>
      try {
        val value = typeTag match {
          case BasicTypes.Long => cp.getLong(index)
          case BasicTypes.Double => cp.getDouble(index)
          case BasicTypes.Float => cp.getFloat(index)
          case _ => cp.getObject(index)
        }
        sb.append(s" ($value)")
      } catch {
        case _: IndexOutOfBoundsException =>
          sb.append(" (OUT OF BOUNDS)")
      }
    }
  }
}
