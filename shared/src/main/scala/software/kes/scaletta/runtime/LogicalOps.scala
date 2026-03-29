package software.kes.scaletta.runtime

import software.kes.scaletta.api.ArgumentReader

object LogicalOps {
  def and(args: ArgumentReader): Boolean =
    args.unsafeReadBoolean(0) &&
      args.unsafeReadThunk[Boolean](1)
        .apply()

  def or(args: ArgumentReader): Boolean =
    args.unsafeReadBoolean(0) ||
      args.unsafeReadThunk[Boolean](1)
        .apply()

  def not(args: ArgumentReader): Boolean =
    !args.unsafeReadBoolean(0)
}
