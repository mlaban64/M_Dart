pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: 10.5.0" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_m_dart" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#9700e8b9#;
   pragma Export (C, u00001, "m_dartB");
   u00002 : constant Version_32 := 16#050ff2f0#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#4113f22b#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#d3f6baa5#;
   pragma Export (C, u00004, "build_functionsB");
   u00005 : constant Version_32 := 16#ed886349#;
   pragma Export (C, u00005, "build_functionsS");
   u00006 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00006, "adaS");
   u00007 : constant Version_32 := 16#f64b89a4#;
   pragma Export (C, u00007, "ada__integer_text_ioB");
   u00008 : constant Version_32 := 16#2ec7c168#;
   pragma Export (C, u00008, "ada__integer_text_ioS");
   u00009 : constant Version_32 := 16#185015e7#;
   pragma Export (C, u00009, "ada__exceptionsB");
   u00010 : constant Version_32 := 16#d6578bab#;
   pragma Export (C, u00010, "ada__exceptionsS");
   u00011 : constant Version_32 := 16#5726abed#;
   pragma Export (C, u00011, "ada__exceptions__last_chance_handlerB");
   u00012 : constant Version_32 := 16#cfec26ee#;
   pragma Export (C, u00012, "ada__exceptions__last_chance_handlerS");
   u00013 : constant Version_32 := 16#4635ec04#;
   pragma Export (C, u00013, "systemS");
   u00014 : constant Version_32 := 16#ae860117#;
   pragma Export (C, u00014, "system__soft_linksB");
   u00015 : constant Version_32 := 16#8d3f9472#;
   pragma Export (C, u00015, "system__soft_linksS");
   u00016 : constant Version_32 := 16#f32b4133#;
   pragma Export (C, u00016, "system__secondary_stackB");
   u00017 : constant Version_32 := 16#03a1141d#;
   pragma Export (C, u00017, "system__secondary_stackS");
   u00018 : constant Version_32 := 16#86dbf443#;
   pragma Export (C, u00018, "system__parametersB");
   u00019 : constant Version_32 := 16#0ed9b82f#;
   pragma Export (C, u00019, "system__parametersS");
   u00020 : constant Version_32 := 16#ced09590#;
   pragma Export (C, u00020, "system__storage_elementsB");
   u00021 : constant Version_32 := 16#6bf6a600#;
   pragma Export (C, u00021, "system__storage_elementsS");
   u00022 : constant Version_32 := 16#75bf515c#;
   pragma Export (C, u00022, "system__soft_links__initializeB");
   u00023 : constant Version_32 := 16#5697fc2b#;
   pragma Export (C, u00023, "system__soft_links__initializeS");
   u00024 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00024, "system__stack_checkingB");
   u00025 : constant Version_32 := 16#c88a87ec#;
   pragma Export (C, u00025, "system__stack_checkingS");
   u00026 : constant Version_32 := 16#34742901#;
   pragma Export (C, u00026, "system__exception_tableB");
   u00027 : constant Version_32 := 16#1b9b8546#;
   pragma Export (C, u00027, "system__exception_tableS");
   u00028 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00028, "system__exceptionsB");
   u00029 : constant Version_32 := 16#2e5681f2#;
   pragma Export (C, u00029, "system__exceptionsS");
   u00030 : constant Version_32 := 16#69416224#;
   pragma Export (C, u00030, "system__exceptions__machineB");
   u00031 : constant Version_32 := 16#5c74e542#;
   pragma Export (C, u00031, "system__exceptions__machineS");
   u00032 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00032, "system__exceptions_debugB");
   u00033 : constant Version_32 := 16#38bf15c0#;
   pragma Export (C, u00033, "system__exceptions_debugS");
   u00034 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00034, "system__img_intB");
   u00035 : constant Version_32 := 16#44ee0cc6#;
   pragma Export (C, u00035, "system__img_intS");
   u00036 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00036, "system__tracebackB");
   u00037 : constant Version_32 := 16#181732c0#;
   pragma Export (C, u00037, "system__tracebackS");
   u00038 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00038, "system__traceback_entriesB");
   u00039 : constant Version_32 := 16#466e1a74#;
   pragma Export (C, u00039, "system__traceback_entriesS");
   u00040 : constant Version_32 := 16#448e9548#;
   pragma Export (C, u00040, "system__traceback__symbolicB");
   u00041 : constant Version_32 := 16#46491211#;
   pragma Export (C, u00041, "system__traceback__symbolicS");
   u00042 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00042, "ada__containersS");
   u00043 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00043, "ada__exceptions__tracebackB");
   u00044 : constant Version_32 := 16#ae2d2db5#;
   pragma Export (C, u00044, "ada__exceptions__tracebackS");
   u00045 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00045, "interfacesS");
   u00046 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00046, "interfaces__cB");
   u00047 : constant Version_32 := 16#467817d8#;
   pragma Export (C, u00047, "interfaces__cS");
   u00048 : constant Version_32 := 16#e865e681#;
   pragma Export (C, u00048, "system__bounded_stringsB");
   u00049 : constant Version_32 := 16#31c8cd1d#;
   pragma Export (C, u00049, "system__bounded_stringsS");
   u00050 : constant Version_32 := 16#0062635e#;
   pragma Export (C, u00050, "system__crtlS");
   u00051 : constant Version_32 := 16#bba79bcb#;
   pragma Export (C, u00051, "system__dwarf_linesB");
   u00052 : constant Version_32 := 16#9a78d181#;
   pragma Export (C, u00052, "system__dwarf_linesS");
   u00053 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00053, "ada__charactersS");
   u00054 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00054, "ada__characters__handlingB");
   u00055 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00055, "ada__characters__handlingS");
   u00056 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00056, "ada__characters__latin_1S");
   u00057 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00057, "ada__stringsS");
   u00058 : constant Version_32 := 16#96df1a3f#;
   pragma Export (C, u00058, "ada__strings__mapsB");
   u00059 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00059, "ada__strings__mapsS");
   u00060 : constant Version_32 := 16#5886cb31#;
   pragma Export (C, u00060, "system__bit_opsB");
   u00061 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00061, "system__bit_opsS");
   u00062 : constant Version_32 := 16#72b39087#;
   pragma Export (C, u00062, "system__unsigned_typesS");
   u00063 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00063, "ada__strings__maps__constantsS");
   u00064 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00064, "system__address_imageB");
   u00065 : constant Version_32 := 16#e7d9713e#;
   pragma Export (C, u00065, "system__address_imageS");
   u00066 : constant Version_32 := 16#ec78c2bf#;
   pragma Export (C, u00066, "system__img_unsB");
   u00067 : constant Version_32 := 16#ed47ac70#;
   pragma Export (C, u00067, "system__img_unsS");
   u00068 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00068, "system__ioB");
   u00069 : constant Version_32 := 16#d8771b4b#;
   pragma Export (C, u00069, "system__ioS");
   u00070 : constant Version_32 := 16#f790d1ef#;
   pragma Export (C, u00070, "system__mmapB");
   u00071 : constant Version_32 := 16#7c445363#;
   pragma Export (C, u00071, "system__mmapS");
   u00072 : constant Version_32 := 16#92d882c5#;
   pragma Export (C, u00072, "ada__io_exceptionsS");
   u00073 : constant Version_32 := 16#91eaca2e#;
   pragma Export (C, u00073, "system__mmap__os_interfaceB");
   u00074 : constant Version_32 := 16#1fc2f713#;
   pragma Export (C, u00074, "system__mmap__os_interfaceS");
   u00075 : constant Version_32 := 16#1e7d913a#;
   pragma Export (C, u00075, "system__mmap__unixS");
   u00076 : constant Version_32 := 16#54420b60#;
   pragma Export (C, u00076, "system__os_libB");
   u00077 : constant Version_32 := 16#d872da39#;
   pragma Export (C, u00077, "system__os_libS");
   u00078 : constant Version_32 := 16#ec4d5631#;
   pragma Export (C, u00078, "system__case_utilB");
   u00079 : constant Version_32 := 16#79e05a50#;
   pragma Export (C, u00079, "system__case_utilS");
   u00080 : constant Version_32 := 16#2a8e89ad#;
   pragma Export (C, u00080, "system__stringsB");
   u00081 : constant Version_32 := 16#2623c091#;
   pragma Export (C, u00081, "system__stringsS");
   u00082 : constant Version_32 := 16#5a3f5337#;
   pragma Export (C, u00082, "system__object_readerB");
   u00083 : constant Version_32 := 16#82413105#;
   pragma Export (C, u00083, "system__object_readerS");
   u00084 : constant Version_32 := 16#fb020d94#;
   pragma Export (C, u00084, "system__val_lliB");
   u00085 : constant Version_32 := 16#2a5b7ef4#;
   pragma Export (C, u00085, "system__val_lliS");
   u00086 : constant Version_32 := 16#b8e72903#;
   pragma Export (C, u00086, "system__val_lluB");
   u00087 : constant Version_32 := 16#1f7d1d65#;
   pragma Export (C, u00087, "system__val_lluS");
   u00088 : constant Version_32 := 16#269742a9#;
   pragma Export (C, u00088, "system__val_utilB");
   u00089 : constant Version_32 := 16#ea955afa#;
   pragma Export (C, u00089, "system__val_utilS");
   u00090 : constant Version_32 := 16#d7bf3f29#;
   pragma Export (C, u00090, "system__exception_tracesB");
   u00091 : constant Version_32 := 16#62eacc9e#;
   pragma Export (C, u00091, "system__exception_tracesS");
   u00092 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00092, "system__wch_conB");
   u00093 : constant Version_32 := 16#5d48ced6#;
   pragma Export (C, u00093, "system__wch_conS");
   u00094 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00094, "system__wch_stwB");
   u00095 : constant Version_32 := 16#7059e2d7#;
   pragma Export (C, u00095, "system__wch_stwS");
   u00096 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00096, "system__wch_cnvB");
   u00097 : constant Version_32 := 16#52ff7425#;
   pragma Export (C, u00097, "system__wch_cnvS");
   u00098 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00098, "system__wch_jisB");
   u00099 : constant Version_32 := 16#d28f6d04#;
   pragma Export (C, u00099, "system__wch_jisS");
   u00100 : constant Version_32 := 16#f4e097a7#;
   pragma Export (C, u00100, "ada__text_ioB");
   u00101 : constant Version_32 := 16#777d5329#;
   pragma Export (C, u00101, "ada__text_ioS");
   u00102 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00102, "ada__streamsB");
   u00103 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00103, "ada__streamsS");
   u00104 : constant Version_32 := 16#5d91da9f#;
   pragma Export (C, u00104, "ada__tagsB");
   u00105 : constant Version_32 := 16#12a0afb8#;
   pragma Export (C, u00105, "ada__tagsS");
   u00106 : constant Version_32 := 16#796f31f1#;
   pragma Export (C, u00106, "system__htableB");
   u00107 : constant Version_32 := 16#c2f75fee#;
   pragma Export (C, u00107, "system__htableS");
   u00108 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00108, "system__string_hashB");
   u00109 : constant Version_32 := 16#60a93490#;
   pragma Export (C, u00109, "system__string_hashS");
   u00110 : constant Version_32 := 16#73d2d764#;
   pragma Export (C, u00110, "interfaces__c_streamsB");
   u00111 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00111, "interfaces__c_streamsS");
   u00112 : constant Version_32 := 16#ec9c64c3#;
   pragma Export (C, u00112, "system__file_ioB");
   u00113 : constant Version_32 := 16#e1440d61#;
   pragma Export (C, u00113, "system__file_ioS");
   u00114 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00114, "ada__finalizationS");
   u00115 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00115, "system__finalization_rootB");
   u00116 : constant Version_32 := 16#09c79f94#;
   pragma Export (C, u00116, "system__finalization_rootS");
   u00117 : constant Version_32 := 16#bbaa76ac#;
   pragma Export (C, u00117, "system__file_control_blockS");
   u00118 : constant Version_32 := 16#fdedfd10#;
   pragma Export (C, u00118, "ada__text_io__integer_auxB");
   u00119 : constant Version_32 := 16#2fe01d89#;
   pragma Export (C, u00119, "ada__text_io__integer_auxS");
   u00120 : constant Version_32 := 16#181dc502#;
   pragma Export (C, u00120, "ada__text_io__generic_auxB");
   u00121 : constant Version_32 := 16#305a076a#;
   pragma Export (C, u00121, "ada__text_io__generic_auxS");
   u00122 : constant Version_32 := 16#b10ba0c7#;
   pragma Export (C, u00122, "system__img_biuB");
   u00123 : constant Version_32 := 16#b49118ca#;
   pragma Export (C, u00123, "system__img_biuS");
   u00124 : constant Version_32 := 16#4e06ab0c#;
   pragma Export (C, u00124, "system__img_llbB");
   u00125 : constant Version_32 := 16#f5560834#;
   pragma Export (C, u00125, "system__img_llbS");
   u00126 : constant Version_32 := 16#9dca6636#;
   pragma Export (C, u00126, "system__img_lliB");
   u00127 : constant Version_32 := 16#577ab9d5#;
   pragma Export (C, u00127, "system__img_lliS");
   u00128 : constant Version_32 := 16#a756d097#;
   pragma Export (C, u00128, "system__img_llwB");
   u00129 : constant Version_32 := 16#5c3a2ba2#;
   pragma Export (C, u00129, "system__img_llwS");
   u00130 : constant Version_32 := 16#eb55dfbb#;
   pragma Export (C, u00130, "system__img_wiuB");
   u00131 : constant Version_32 := 16#dad09f58#;
   pragma Export (C, u00131, "system__img_wiuS");
   u00132 : constant Version_32 := 16#0f9783a4#;
   pragma Export (C, u00132, "system__val_intB");
   u00133 : constant Version_32 := 16#f3ca8567#;
   pragma Export (C, u00133, "system__val_intS");
   u00134 : constant Version_32 := 16#383fd226#;
   pragma Export (C, u00134, "system__val_unsB");
   u00135 : constant Version_32 := 16#47b5ed3e#;
   pragma Export (C, u00135, "system__val_unsS");
   u00136 : constant Version_32 := 16#cd2959fb#;
   pragma Export (C, u00136, "ada__numericsS");
   u00137 : constant Version_32 := 16#d976e2b4#;
   pragma Export (C, u00137, "ada__numerics__float_randomB");
   u00138 : constant Version_32 := 16#62aa8dd2#;
   pragma Export (C, u00138, "ada__numerics__float_randomS");
   u00139 : constant Version_32 := 16#ec9cfed1#;
   pragma Export (C, u00139, "system__random_numbersB");
   u00140 : constant Version_32 := 16#852d5c9e#;
   pragma Export (C, u00140, "system__random_numbersS");
   u00141 : constant Version_32 := 16#15692802#;
   pragma Export (C, u00141, "system__random_seedB");
   u00142 : constant Version_32 := 16#1d25c55f#;
   pragma Export (C, u00142, "system__random_seedS");
   u00143 : constant Version_32 := 16#6feb5362#;
   pragma Export (C, u00143, "ada__calendarB");
   u00144 : constant Version_32 := 16#31350a81#;
   pragma Export (C, u00144, "ada__calendarS");
   u00145 : constant Version_32 := 16#51f2d040#;
   pragma Export (C, u00145, "system__os_primitivesB");
   u00146 : constant Version_32 := 16#41c889f2#;
   pragma Export (C, u00146, "system__os_primitivesS");
   u00147 : constant Version_32 := 16#43dea28b#;
   pragma Export (C, u00147, "camerasB");
   u00148 : constant Version_32 := 16#d0825e52#;
   pragma Export (C, u00148, "camerasS");
   u00149 : constant Version_32 := 16#5a895de2#;
   pragma Export (C, u00149, "system__pool_globalB");
   u00150 : constant Version_32 := 16#7141203e#;
   pragma Export (C, u00150, "system__pool_globalS");
   u00151 : constant Version_32 := 16#e31b7c4e#;
   pragma Export (C, u00151, "system__memoryB");
   u00152 : constant Version_32 := 16#1f488a30#;
   pragma Export (C, u00152, "system__memoryS");
   u00153 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00153, "system__storage_poolsB");
   u00154 : constant Version_32 := 16#65d872a9#;
   pragma Export (C, u00154, "system__storage_poolsS");
   u00155 : constant Version_32 := 16#a02f73f2#;
   pragma Export (C, u00155, "system__storage_pools__subpoolsB");
   u00156 : constant Version_32 := 16#cc5a1856#;
   pragma Export (C, u00156, "system__storage_pools__subpoolsS");
   u00157 : constant Version_32 := 16#57674f80#;
   pragma Export (C, u00157, "system__finalization_mastersB");
   u00158 : constant Version_32 := 16#1dc9d5ce#;
   pragma Export (C, u00158, "system__finalization_mastersS");
   u00159 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00159, "system__img_boolB");
   u00160 : constant Version_32 := 16#b3ec9def#;
   pragma Export (C, u00160, "system__img_boolS");
   u00161 : constant Version_32 := 16#84042202#;
   pragma Export (C, u00161, "system__storage_pools__subpools__finalizationB");
   u00162 : constant Version_32 := 16#fe2f4b3a#;
   pragma Export (C, u00162, "system__storage_pools__subpools__finalizationS");
   u00163 : constant Version_32 := 16#4fbc9dd0#;
   pragma Export (C, u00163, "tracersB");
   u00164 : constant Version_32 := 16#4cc484ee#;
   pragma Export (C, u00164, "tracersS");
   u00165 : constant Version_32 := 16#cfee537f#;
   pragma Export (C, u00165, "hitpointsB");
   u00166 : constant Version_32 := 16#4f1486d9#;
   pragma Export (C, u00166, "hitpointsS");
   u00167 : constant Version_32 := 16#faacbed4#;
   pragma Export (C, u00167, "large_float_functionsB");
   u00168 : constant Version_32 := 16#42544a7c#;
   pragma Export (C, u00168, "large_float_functionsS");
   u00169 : constant Version_32 := 16#e5114ee9#;
   pragma Export (C, u00169, "ada__numerics__auxB");
   u00170 : constant Version_32 := 16#9f6e24ed#;
   pragma Export (C, u00170, "ada__numerics__auxS");
   u00171 : constant Version_32 := 16#42a257f7#;
   pragma Export (C, u00171, "system__fat_llfS");
   u00172 : constant Version_32 := 16#2b5d4b05#;
   pragma Export (C, u00172, "system__machine_codeS");
   u00173 : constant Version_32 := 16#4ec47af8#;
   pragma Export (C, u00173, "core_typesB");
   u00174 : constant Version_32 := 16#56d852cb#;
   pragma Export (C, u00174, "core_typesS");
   u00175 : constant Version_32 := 16#e18a47a0#;
   pragma Export (C, u00175, "ada__float_text_ioB");
   u00176 : constant Version_32 := 16#39060f6c#;
   pragma Export (C, u00176, "ada__float_text_ioS");
   u00177 : constant Version_32 := 16#25afee5b#;
   pragma Export (C, u00177, "ada__text_io__float_auxB");
   u00178 : constant Version_32 := 16#6ecdea4c#;
   pragma Export (C, u00178, "ada__text_io__float_auxS");
   u00179 : constant Version_32 := 16#8aa4f090#;
   pragma Export (C, u00179, "system__img_realB");
   u00180 : constant Version_32 := 16#819dbde6#;
   pragma Export (C, u00180, "system__img_realS");
   u00181 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00181, "system__float_controlB");
   u00182 : constant Version_32 := 16#a6c9af38#;
   pragma Export (C, u00182, "system__float_controlS");
   u00183 : constant Version_32 := 16#3e932977#;
   pragma Export (C, u00183, "system__img_lluB");
   u00184 : constant Version_32 := 16#3b7a9044#;
   pragma Export (C, u00184, "system__img_lluS");
   u00185 : constant Version_32 := 16#16458a73#;
   pragma Export (C, u00185, "system__powten_tableS");
   u00186 : constant Version_32 := 16#6620fa49#;
   pragma Export (C, u00186, "system__val_realB");
   u00187 : constant Version_32 := 16#484a00d1#;
   pragma Export (C, u00187, "system__val_realS");
   u00188 : constant Version_32 := 16#b2a569d2#;
   pragma Export (C, u00188, "system__exn_llfB");
   u00189 : constant Version_32 := 16#fa4b57d8#;
   pragma Export (C, u00189, "system__exn_llfS");
   u00190 : constant Version_32 := 16#1e40f010#;
   pragma Export (C, u00190, "system__fat_fltS");
   u00191 : constant Version_32 := 16#4969a46f#;
   pragma Export (C, u00191, "ada__long_float_text_ioB");
   u00192 : constant Version_32 := 16#91e5eca3#;
   pragma Export (C, u00192, "ada__long_float_text_ioS");
   u00193 : constant Version_32 := 16#3872f91d#;
   pragma Export (C, u00193, "system__fat_lfltS");
   u00194 : constant Version_32 := 16#09572056#;
   pragma Export (C, u00194, "ada__long_integer_text_ioB");
   u00195 : constant Version_32 := 16#d1db689a#;
   pragma Export (C, u00195, "ada__long_integer_text_ioS");
   u00196 : constant Version_32 := 16#9009cfb3#;
   pragma Export (C, u00196, "ada__long_long_float_text_ioB");
   u00197 : constant Version_32 := 16#4885877f#;
   pragma Export (C, u00197, "ada__long_long_float_text_ioS");
   u00198 : constant Version_32 := 16#9aecf52b#;
   pragma Export (C, u00198, "ada__long_long_integer_text_ioB");
   u00199 : constant Version_32 := 16#4260bde7#;
   pragma Export (C, u00199, "ada__long_long_integer_text_ioS");
   u00200 : constant Version_32 := 16#36972c05#;
   pragma Export (C, u00200, "normal_float_functionsB");
   u00201 : constant Version_32 := 16#8e6fd8ad#;
   pragma Export (C, u00201, "normal_float_functionsS");
   u00202 : constant Version_32 := 16#5c6941bd#;
   pragma Export (C, u00202, "small_float_functionsB");
   u00203 : constant Version_32 := 16#e491b515#;
   pragma Export (C, u00203, "small_float_functionsS");
   u00204 : constant Version_32 := 16#2e42d137#;
   pragma Export (C, u00204, "linear_mathB");
   u00205 : constant Version_32 := 16#02231e92#;
   pragma Export (C, u00205, "linear_mathS");
   u00206 : constant Version_32 := 16#96c5ec9d#;
   pragma Export (C, u00206, "objectsB");
   u00207 : constant Version_32 := 16#567e90d9#;
   pragma Export (C, u00207, "objectsS");
   u00208 : constant Version_32 := 16#154afd9d#;
   pragma Export (C, u00208, "utilitiesB");
   u00209 : constant Version_32 := 16#f97bebdc#;
   pragma Export (C, u00209, "utilitiesS");
   u00210 : constant Version_32 := 16#26518ca7#;
   pragma Export (C, u00210, "ada__calendar__formattingB");
   u00211 : constant Version_32 := 16#0dbf7387#;
   pragma Export (C, u00211, "ada__calendar__formattingS");
   u00212 : constant Version_32 := 16#e3cca715#;
   pragma Export (C, u00212, "ada__calendar__time_zonesB");
   u00213 : constant Version_32 := 16#07d0e97b#;
   pragma Export (C, u00213, "ada__calendar__time_zonesS");
   u00214 : constant Version_32 := 16#08e5ea36#;
   pragma Export (C, u00214, "scenesB");
   u00215 : constant Version_32 := 16#d99507fa#;
   pragma Export (C, u00215, "scenesS");
   u00216 : constant Version_32 := 16#a0e0f569#;
   pragma Export (C, u00216, "m_graphixB");
   u00217 : constant Version_32 := 16#bd2eb84a#;
   pragma Export (C, u00217, "m_graphixS");
   u00218 : constant Version_32 := 16#351539c5#;
   pragma Export (C, u00218, "ada__strings__unboundedB");
   u00219 : constant Version_32 := 16#6552cb60#;
   pragma Export (C, u00219, "ada__strings__unboundedS");
   u00220 : constant Version_32 := 16#60da0992#;
   pragma Export (C, u00220, "ada__strings__searchB");
   u00221 : constant Version_32 := 16#c1ab8667#;
   pragma Export (C, u00221, "ada__strings__searchS");
   u00222 : constant Version_32 := 16#acee74ad#;
   pragma Export (C, u00222, "system__compare_array_unsigned_8B");
   u00223 : constant Version_32 := 16#ef369d89#;
   pragma Export (C, u00223, "system__compare_array_unsigned_8S");
   u00224 : constant Version_32 := 16#a8025f3c#;
   pragma Export (C, u00224, "system__address_operationsB");
   u00225 : constant Version_32 := 16#55395237#;
   pragma Export (C, u00225, "system__address_operationsS");
   u00226 : constant Version_32 := 16#020a3f4d#;
   pragma Export (C, u00226, "system__atomic_countersB");
   u00227 : constant Version_32 := 16#f269c189#;
   pragma Export (C, u00227, "system__atomic_countersS");
   u00228 : constant Version_32 := 16#039168f8#;
   pragma Export (C, u00228, "system__stream_attributesB");
   u00229 : constant Version_32 := 16#8bc30a4e#;
   pragma Export (C, u00229, "system__stream_attributesS");
   u00230 : constant Version_32 := 16#e66dece1#;
   pragma Export (C, u00230, "tone_mapsB");
   u00231 : constant Version_32 := 16#99364177#;
   pragma Export (C, u00231, "tone_mapsS");
   u00232 : constant Version_32 := 16#1538673f#;
   pragma Export (C, u00232, "spectraB");
   u00233 : constant Version_32 := 16#3eb5748b#;
   pragma Export (C, u00233, "spectraS");
   u00234 : constant Version_32 := 16#5bbc69da#;
   pragma Export (C, u00234, "lightsB");
   u00235 : constant Version_32 := 16#1850406e#;
   pragma Export (C, u00235, "lightsS");
   u00236 : constant Version_32 := 16#1e73eec5#;
   pragma Export (C, u00236, "samplersB");
   u00237 : constant Version_32 := 16#9b97e271#;
   pragma Export (C, u00237, "samplersS");
   u00238 : constant Version_32 := 16#673c2cfb#;
   pragma Export (C, u00238, "materialsB");
   u00239 : constant Version_32 := 16#5675244f#;
   pragma Export (C, u00239, "materialsS");
   u00240 : constant Version_32 := 16#203b8c49#;
   pragma Export (C, u00240, "system__strings__stream_opsB");
   u00241 : constant Version_32 := 16#ec029138#;
   pragma Export (C, u00241, "system__strings__stream_opsS");
   u00242 : constant Version_32 := 16#3a26b1ba#;
   pragma Export (C, u00242, "shadepointsB");
   u00243 : constant Version_32 := 16#c622a450#;
   pragma Export (C, u00243, "shadepointsS");
   u00244 : constant Version_32 := 16#6ad66c6d#;
   pragma Export (C, u00244, "cameras__pinhole_camerasB");
   u00245 : constant Version_32 := 16#f74bd523#;
   pragma Export (C, u00245, "cameras__pinhole_camerasS");
   u00246 : constant Version_32 := 16#786a8938#;
   pragma Export (C, u00246, "lights__directionalsB");
   u00247 : constant Version_32 := 16#a6296f48#;
   pragma Export (C, u00247, "lights__directionalsS");
   u00248 : constant Version_32 := 16#8f461df5#;
   pragma Export (C, u00248, "text_ioS");
   u00249 : constant Version_32 := 16#9b8e7af3#;
   pragma Export (C, u00249, "lights__pointsB");
   u00250 : constant Version_32 := 16#cc37f903#;
   pragma Export (C, u00250, "lights__pointsS");
   u00251 : constant Version_32 := 16#3b1092f0#;
   pragma Export (C, u00251, "materials__lambertianB");
   u00252 : constant Version_32 := 16#6dd12c70#;
   pragma Export (C, u00252, "materials__lambertianS");
   u00253 : constant Version_32 := 16#1b91ccaa#;
   pragma Export (C, u00253, "materials__phongB");
   u00254 : constant Version_32 := 16#efc0f7f4#;
   pragma Export (C, u00254, "materials__phongS");
   u00255 : constant Version_32 := 16#64a62a37#;
   pragma Export (C, u00255, "materials__reflectiveB");
   u00256 : constant Version_32 := 16#85401bc6#;
   pragma Export (C, u00256, "materials__reflectiveS");
   u00257 : constant Version_32 := 16#dc33a279#;
   pragma Export (C, u00257, "materials__transparentB");
   u00258 : constant Version_32 := 16#e099dc70#;
   pragma Export (C, u00258, "materials__transparentS");
   u00259 : constant Version_32 := 16#f5314850#;
   pragma Export (C, u00259, "objects__compoundsB");
   u00260 : constant Version_32 := 16#eee08711#;
   pragma Export (C, u00260, "objects__compoundsS");
   u00261 : constant Version_32 := 16#ce3b099c#;
   pragma Export (C, u00261, "objects__csg_objectsB");
   u00262 : constant Version_32 := 16#a60f2e7b#;
   pragma Export (C, u00262, "objects__csg_objectsS");
   u00263 : constant Version_32 := 16#f713106f#;
   pragma Export (C, u00263, "objects__unit_conesB");
   u00264 : constant Version_32 := 16#729060de#;
   pragma Export (C, u00264, "objects__unit_conesS");
   u00265 : constant Version_32 := 16#6cbe158b#;
   pragma Export (C, u00265, "objects__unit_cubesB");
   u00266 : constant Version_32 := 16#63b88b3c#;
   pragma Export (C, u00266, "objects__unit_cubesS");
   u00267 : constant Version_32 := 16#94d2cc7c#;
   pragma Export (C, u00267, "objects__unit_cylindersB");
   u00268 : constant Version_32 := 16#146e9135#;
   pragma Export (C, u00268, "objects__unit_cylindersS");
   u00269 : constant Version_32 := 16#1f7e0dae#;
   pragma Export (C, u00269, "objects__unit_spheresB");
   u00270 : constant Version_32 := 16#fe796e0f#;
   pragma Export (C, u00270, "objects__unit_spheresS");
   u00271 : constant Version_32 := 16#adee51e1#;
   pragma Export (C, u00271, "objects__trianglesB");
   u00272 : constant Version_32 := 16#bc02f24d#;
   pragma Export (C, u00272, "objects__trianglesS");
   u00273 : constant Version_32 := 16#b17fb37f#;
   pragma Export (C, u00273, "samplers__unitsquaresB");
   u00274 : constant Version_32 := 16#c508287e#;
   pragma Export (C, u00274, "samplers__unitsquaresS");
   u00275 : constant Version_32 := 16#2f4435b2#;
   pragma Export (C, u00275, "core_testsB");
   u00276 : constant Version_32 := 16#e36238e3#;
   pragma Export (C, u00276, "core_testsS");
   u00277 : constant Version_32 := 16#ec87c64c#;
   pragma Export (C, u00277, "illustrationsB");
   u00278 : constant Version_32 := 16#76d19895#;
   pragma Export (C, u00278, "illustrationsS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.atomic_counters%s
   --  system.atomic_counters%b
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.machine_code%s
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.img_biu%s
   --  system.img_biu%b
   --  system.img_llb%s
   --  system.img_llb%b
   --  system.img_llu%s
   --  system.img_llu%b
   --  system.img_llw%s
   --  system.img_llw%b
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.img_wiu%s
   --  system.img_wiu%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.traceback%s
   --  system.traceback%b
   --  ada.characters.handling%s
   --  system.case_util%s
   --  system.os_lib%s
   --  system.secondary_stack%s
   --  system.standard_library%s
   --  ada.exceptions%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.soft_links%s
   --  system.val_lli%s
   --  system.val_llu%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  ada.exceptions.traceback%b
   --  system.address_image%s
   --  system.address_image%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.bounded_strings%s
   --  system.bounded_strings%b
   --  system.case_util%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.maps.constants%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  ada.characters.handling%b
   --  system.exception_traces%s
   --  system.exception_traces%b
   --  system.memory%s
   --  system.memory%b
   --  system.mmap%s
   --  system.mmap.os_interface%s
   --  system.mmap%b
   --  system.mmap.unix%s
   --  system.mmap.os_interface%b
   --  system.object_reader%s
   --  system.object_reader%b
   --  system.dwarf_lines%s
   --  system.dwarf_lines%b
   --  system.os_lib%b
   --  system.secondary_stack%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  system.standard_library%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  system.val_lli%b
   --  system.val_llu%b
   --  ada.numerics%s
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.fat_flt%s
   --  system.fat_lflt%s
   --  system.fat_llf%s
   --  ada.numerics.aux%s
   --  ada.numerics.aux%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.img_real%s
   --  system.img_real%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.finalization_masters%b
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.storage_pools.subpools%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  system.val_real%s
   --  system.val_real%b
   --  system.val_uns%s
   --  system.val_uns%b
   --  system.val_int%s
   --  system.val_int%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.time_zones%s
   --  ada.calendar.time_zones%b
   --  ada.calendar.formatting%s
   --  ada.calendar.formatting%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  ada.text_io.generic_aux%s
   --  ada.text_io.generic_aux%b
   --  ada.text_io.float_aux%s
   --  ada.text_io.float_aux%b
   --  ada.float_text_io%s
   --  ada.float_text_io%b
   --  ada.long_float_text_io%s
   --  ada.long_float_text_io%b
   --  ada.long_long_float_text_io%s
   --  ada.long_long_float_text_io%b
   --  ada.text_io.integer_aux%s
   --  ada.text_io.integer_aux%b
   --  ada.integer_text_io%s
   --  ada.integer_text_io%b
   --  ada.long_integer_text_io%s
   --  ada.long_integer_text_io%b
   --  ada.long_long_integer_text_io%s
   --  ada.long_long_integer_text_io%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.random_seed%s
   --  system.random_seed%b
   --  system.random_numbers%s
   --  system.random_numbers%b
   --  ada.numerics.float_random%s
   --  ada.numerics.float_random%b
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  text_io%s
   --  core_types%s
   --  core_types%b
   --  large_float_functions%s
   --  large_float_functions%b
   --  m_graphix%s
   --  m_graphix%b
   --  normal_float_functions%s
   --  normal_float_functions%b
   --  small_float_functions%s
   --  small_float_functions%b
   --  linear_math%s
   --  linear_math%b
   --  hitpoints%s
   --  hitpoints%b
   --  samplers%s
   --  spectra%s
   --  cameras%s
   --  lights%s
   --  lights%b
   --  materials%s
   --  objects%s
   --  scenes%s
   --  tone_maps%s
   --  tracers%s
   --  utilities%s
   --  utilities%b
   --  cameras%b
   --  materials%b
   --  objects%b
   --  scenes%b
   --  shadepoints%s
   --  shadepoints%b
   --  samplers%b
   --  spectra%b
   --  tone_maps%b
   --  tracers%b
   --  cameras.pinhole_cameras%s
   --  cameras.pinhole_cameras%b
   --  lights.directionals%s
   --  lights.directionals%b
   --  lights.points%s
   --  lights.points%b
   --  materials.lambertian%s
   --  materials.lambertian%b
   --  materials.phong%s
   --  materials.phong%b
   --  materials.reflective%s
   --  materials.reflective%b
   --  materials.transparent%s
   --  materials.transparent%b
   --  objects.csg_objects%s
   --  objects.csg_objects%b
   --  objects.triangles%s
   --  objects.triangles%b
   --  objects.unit_cones%s
   --  objects.unit_cones%b
   --  objects.unit_cubes%s
   --  objects.unit_cubes%b
   --  objects.unit_cylinders%s
   --  objects.unit_cylinders%b
   --  objects.unit_spheres%s
   --  objects.unit_spheres%b
   --  core_tests%s
   --  core_tests%b
   --  objects.compounds%s
   --  objects.compounds%b
   --  samplers.unitsquares%s
   --  samplers.unitsquares%b
   --  build_functions%s
   --  build_functions%b
   --  illustrations%s
   --  illustrations%b
   --  m_dart%b
   --  END ELABORATION ORDER

end ada_main;
