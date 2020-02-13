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
                                      , TopLevel(..)
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
-- import from stdcxx
-- -------------------------------------------------------------------

stdcxx_cabal :: Cabal
stdcxx_cabal = Cabal {
    cabal_pkgname            = CabalName "stdcxx"
  , cabal_version            = "0.6"
  , cabal_cheaderprefix      = "STD"
  , cabal_moduleprefix       = "STD"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = []
  , cabal_license            = Nothing
  , cabal_licensefile        = Nothing
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = []
  , cabal_buildType          = Simple
  }

-- import from stdcxx
deletable :: Class
deletable =
  AbstractClass {
    class_cabal      = stdcxx_cabal
  , class_name       = "Deletable"
  , class_parents    = []
  , class_protected  = Protected []
  , class_alias      = Nothing
  , class_funcs      = [ Destructor Nothing ]
  , class_vars       = []
  , class_tmpl_funcs = []
  }

t_vector :: TemplateClass
t_vector = TmplCls stdcxx_cabal "Vector" (FormSimple "std::vector") ["tp1"]
             [ TFunNew [] Nothing
             , TFun void_ "push_back" "push_back"   [Arg (TemplateParam "tp1") "x"]
             , TFun void_ "pop_back"  "pop_back"    []
             , TFun (TemplateParam "tp1") "at" "at" [int "n"]
             , TFun int_  "size"      "size"        []
             , TFunDelete
             ]
             []

-- -------------------------------------------------------------------
-- HDataFrame
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


t_StdDataFrame :: TemplateClass
t_StdDataFrame =
  TmplCls cabal "StdDataFrame" (FormSimple "hmdf::vector") ["tp1"]
    []
    []


extraDep :: [(String,[String])]
extraDep = []

extraLib :: [String]
extraLib = [ "rt" ]

classes :: [Class]
classes = []

toplevel :: [TopLevel]
toplevel =
  [ TLTemplate
      (TopLevelTemplateFunction {
         topleveltfunc_ret   = TemplateAppMove (TemplateAppInfo t_vector [TArg_TypeParam "t1"] "std::vector")
       , topleveltfunc_name  = "gen_uniform_real_dist"
       , topleveltfunc_oname = "gen_uniform_real_dist"
       , topleveltfunc_args  = [int "n"]
       }
      )
  ]

templates :: [TemplateClassImportHeader]
templates =
  [ TCIH t_StdDataFrame ["DataFrame/DataFrameTypes.h"]
  ]

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
               , sbcTopLevels  = toplevel
               , sbcTemplates  = templates
               , sbcExtraLibs  = extraLib
               , sbcExtraDeps  = extraDep
               , sbcStaticFiles = []
               }

  simpleBuilder fficfg sbcfg
