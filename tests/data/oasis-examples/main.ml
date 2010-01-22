
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
