(********************************************************************************)
(*  ODN: Dump data using OCaml notation                                         *)
(*                                                                              *)
(*  Copyright (C) 2009-2011, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

open OASISTypes;;

let pkg = 
  {
    oasis_version =  "1.0";
    ocaml_version =  None;
    name =           "mypkg";
    version =        "1.0";
    license =        LGPL;
    license_file =   "LICENSE.txt";
    copyrights =     [];
    maintainers =    [];
    authors =        [];
    homepage =       None;
    synopsis =       "test synopsis";
    description =    None;
    categories =     [];
    build_depends =  [];
    build_tools =    [];
    conf_type =      "internal";
    build_type =     "internal";
    install_type =   "internal";
    files_ab =       [];
    plugins =        [];
    libraries =      [];
    executables =    [];
    flags =          [];
    src_repos =      [];
    tests =          [];
    schema_data =    PropList.Data.create ();
  }

let () = 
  print_endline (ODN.string_of_odn (odn_of_package pkg))
