module Flora.TemplateSpec where

import qualified Distribution.Version as Version
import Servant

import Flora.Model.Package
import Flora.TestUtils (TestM, TestTree, assertEqual, testThese, testThis)
import qualified FloraWeb.Links as Links

spec :: TestM TestTree
spec =
  testThese
    "templates"
    [ testThis "Generate a link to a package + version" testGenerateVersionedPackageLink
    ]

testGenerateVersionedPackageLink :: TestM ()
testGenerateVersionedPackageLink = do
  let namespace = Namespace "hackage"
  let packageName = PackageName "base"
  let version = Version.mkVersion [4, 16, 1, 0]
  let generatedLink = toUrlPiece $ Links.packageVersionLink namespace packageName version
  assertEqual "packages/%40hackage/base/4.16.1.0" generatedLink
