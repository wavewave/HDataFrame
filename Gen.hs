{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
--
import FFICXX.Generate.Builder        ( simpleBuilder )
import FFICXX.Generate.Code.Primitive ( bool_
                                      , charpp
                                      , cppclass, cppclass_
                                      , cstring, cstring_
                                      , double, double_
                                      , int, int_
                                      , uint, uint_
                                      , void_, voidp
                                      )
import FFICXX.Generate.Config         ( FFICXXConfig(..)
                                      , SimpleBuilderConfig(..)
                                      )
import FFICXX.Generate.Type.Cabal     ( BuildType(..), Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..), ModuleUnitMap(..), ModuleUnitImports(..)
                                      , modImports
                                      )
import FFICXX.Generate.Type.Class     ( Arg(..)
                                      , Class(..)
                                      , CTypes(CTDouble)
                                      , Form(FormSimple)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevelFunction(..)
                                      , Variable(..)
                                      )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..)
                                      , ModuleUnitImports(..)
                                      )
import FFICXX.Runtime.CodeGen.Cxx     ( Namespace(..), HeaderName(..) )



import qualified Data.HashMap.Strict as HM (fromList)
import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Code.Primitive
import FFICXX.Generate.Type.Cabal     ( AddCInc(..)
                                      , AddCSrc(..)
                                      , BuildType(Simple)
                                      , Cabal(..)
                                      , CabalName(..)
                                      )
import FFICXX.Generate.Type.Config (ModuleUnit(..),ModuleUnitMap(..)
                                   ,ModuleUnitImports(..))
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface


-- -------------------------------------------------------------------
-- tmpl-dep-test
-- -------------------------------------------------------------------

cabal :: Cabal
cabal =
  Cabal { cabal_pkgname            = CabalName "HDataFrame"
        , cabal_version            = "0.0"
        , cabal_cheaderprefix      = "HDataFrame"
        , cabal_moduleprefix       = "HDataFrame"
        , cabal_additional_c_incs  = []
        , cabal_additional_c_srcs  = []
        , cabal_additional_pkgdeps = [ CabalName "stdcxx" ]
        , cabal_license            = Just "BSD3"
        , cabal_licensefile        = Just "LICENSE"
        , cabal_extraincludedirs   = []
        , cabal_extralibdirs       = []
        , cabal_extrafiles         = []
        , cabal_pkg_config_depends = [ "DataFrame" ]
        , cabal_buildType          = Simple
        }

extraDep :: [(String,[String])]
extraDep = []

extraLib :: [String]
extraLib = [ "rt" ]


classes :: [Class]
classes = []

toplevelfunctions :: [TopLevelFunction]
toplevelfunctions = []

templates :: [TemplateClassImportHeader]
templates = []

headers :: [(ModuleUnit, ModuleUnitImports)]
headers =
  []

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let fficfg = FFICXXConfig {
                 fficxxconfig_workingDir     = cwd </> "working"
               , fficxxconfig_installBaseDir = cwd </> "HDataFrame"
               , fficxxconfig_staticFileDir  = "../template"
               }
      sbcfg  = SimpleBuilderConfig {
                 sbcTopModule  = "HDataFrame"
               , sbcModUnitMap = ModuleUnitMap (HM.fromList headers)
               , sbcCabal      = cabal
               , sbcClasses    = classes
               , sbcTopLevels  = toplevelfunctions
               , sbcTemplates  = templates
               , sbcExtraLibs  = extraLib
               , sbcExtraDeps  = extraDep
               , sbcStaticFiles = []
               }

  simpleBuilder fficfg sbcfg
