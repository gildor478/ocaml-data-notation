(******************************************************************************)
(* ocaml-data-notation: Store data using OCaml notation                       *)
(*                                                                            *)
(* Copyright (C) 2009-2011, OCamlCore SARL                                    *)
(* Copyright (C) 2013, Sylvain Le Gall                                        *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)

(** OASIS types and exceptions
   @author Sylvain Le Gall
  *)

TYPE_CONV_PATH "OASISTypes";;

(** Alias type
  *)
type name               = string with odn;;
type package_name       = string with odn;;
type url                = string with odn;;
type version            = string with odn;;
type version_constraint = string with odn;;
type dirname            = string with odn;;
type filename           = string with odn;;
type prog               = string with odn;;

(** Valid licenses
  *)
type license =
  | AllRightsReserved
  | BSD3
  | BSD4
  | GPL
  | LGPL
  | LGPL_link_exn
  | Other of url
  | PublicDomain
    with odn
;;

(** Compilation type
  *)
type compiled_object =
  | Byte
  | Native
  | Best
    with odn
;;

(** Package dependency
  *)
type dependency =
  | FindlibPackage of package_name * version_constraint option
  | InternalLibrary of name
    with odn
;;

(** Possible VCS
  *)
type vcs =
  | Darcs
  | Git
  | Svn
  | Cvs
  | Hg
  | Bzr
  | Arch
  | Monotone
    with odn
;;

(** Available test
  *)
type expr_test =
  | TOs_type
  | TSystem
  | TArchitecture
  | TCcomp_type
  | TOCaml_version
    with odn
;;

(** Boolean expression to express condition on values
  *)
type expr =
  | EBool of bool
  | ENot of expr
  | EAnd of expr * expr
  | EOr of expr * expr
  | EFlag of string
  | ETest of expr_test * string
    with odn
;;

(** Conditional value
  *)
type 'a conditional =
    (expr * 'a) list
    with odn
;;

(** Library definition
  *)
type library =
    {
      lib_build:           bool conditional;
      lib_install:         bool conditional;
      lib_path:            dirname;
      lib_modules:         string list;
      lib_compiled_object: compiled_object;
      lib_build_depends:   dependency list;
      lib_build_tools:     prog list;
      lib_c_sources:       filename list;
      lib_data_files:      (filename * filename) list;
      lib_schema_data:     PropList.Data.t;
    } with odn
;;

(** Executable definition
  *)
type executable =
    {
      exec_build:           bool conditional;
      exec_install:         bool conditional;
      exec_main_is:         filename;
      exec_compiled_object: compiled_object;
      exec_build_depends:   dependency list;
      exec_build_tools:     prog list;
      exec_c_sources:       filename list;
      exec_custom:          bool;
      exec_data_files:      (filename * filename) list;
      exec_is:              filename; (* Real executable *)
      exec_schema_data:     PropList.Data.t;
    } with odn
;;

(** Command line flag defintion
  *)
type flag =
    {
      flag_description:  string option;
      flag_default:      bool conditional;
      flag_schema_data:  PropList.Data.t;
    } with odn
;;

(** Source repository definition
  *)
type source_repository =
    {
      src_repo_type:        vcs;
      src_repo_location:    url;
      src_repo_browser:     url option;
      src_repo_module:      string option;
      src_repo_branch:      string option;
      src_repo_tag:         string option;
      src_repo_subdir:      filename option;
      src_repo_schema_data: PropList.Data.t;
    } with odn
;;

(** Test definition
  *)
type test =
    {
      test_type:               string;
      test_command:            string;
      test_working_directory:  filename option;
      test_run:                bool conditional;
      test_build_tools:        prog list;
      test_schema_data:        PropList.Data.t;
    } with odn
;;

(** OASIS file whole content
  *)
type package =
    {
      oasis_version:  version;
      ocaml_version:  version_constraint option;
      name:           package_name;
      version:        version;
      license:        license;
      license_file:   filename;
      copyrights:     string list;
      maintainers:    string list;
      authors:        string list;
      homepage:       url option;
      synopsis:       string;
      description:    string option;
      categories:     url list;
      build_depends:  dependency list;
      build_tools:    prog list;
      conf_type:      string;
      build_type:     string;
      install_type:   string;
      files_ab:       filename list;
      plugins:        string list;
      libraries:      (name * library) list;
      executables:    (name * executable) list;
      flags:          (name * flag) list;
      src_repos:      (name * source_repository) list;
      tests:          (name * test) list;
      schema_data:    PropList.Data.t;
    } with odn
;;

