package software.kes.scaletta.api

object Packages {
  val scaletta: PackagePath.Absolute = PackagePath.root / PackageSegment("scaletta")
  val scalettaCollection: PackagePath.Absolute = scaletta / PackageSegment("collection")
}
