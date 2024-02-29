pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__m_dart.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__m_dart.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E077 : Short_Integer; pragma Import (Ada, E077, "system__os_lib_E");
   E010 : Short_Integer; pragma Import (Ada, E010, "ada__exceptions_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exception_table_E");
   E042 : Short_Integer; pragma Import (Ada, E042, "ada__containers_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "ada__io_exceptions_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "ada__strings_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "ada__strings__maps_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "ada__strings__maps__constants_E");
   E047 : Short_Integer; pragma Import (Ada, E047, "interfaces__c_E");
   E029 : Short_Integer; pragma Import (Ada, E029, "system__exceptions_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "system__object_reader_E");
   E052 : Short_Integer; pragma Import (Ada, E052, "system__dwarf_lines_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__soft_links__initialize_E");
   E041 : Short_Integer; pragma Import (Ada, E041, "system__traceback__symbolic_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "ada__numerics_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "ada__tags_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "ada__streams_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "system__file_control_block_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "system__finalization_root_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "ada__finalization_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "system__file_io_E");
   E154 : Short_Integer; pragma Import (Ada, E154, "system__storage_pools_E");
   E158 : Short_Integer; pragma Import (Ada, E158, "system__finalization_masters_E");
   E156 : Short_Integer; pragma Import (Ada, E156, "system__storage_pools__subpools_E");
   E219 : Short_Integer; pragma Import (Ada, E219, "ada__strings__unbounded_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "ada__calendar_E");
   E213 : Short_Integer; pragma Import (Ada, E213, "ada__calendar__time_zones_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "ada__text_io_E");
   E150 : Short_Integer; pragma Import (Ada, E150, "system__pool_global_E");
   E142 : Short_Integer; pragma Import (Ada, E142, "system__random_seed_E");
   E174 : Short_Integer; pragma Import (Ada, E174, "core_types_E");
   E217 : Short_Integer; pragma Import (Ada, E217, "m_graphix_E");
   E205 : Short_Integer; pragma Import (Ada, E205, "linear_math_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "hitpoints_E");
   E237 : Short_Integer; pragma Import (Ada, E237, "samplers_E");
   E233 : Short_Integer; pragma Import (Ada, E233, "spectra_E");
   E148 : Short_Integer; pragma Import (Ada, E148, "cameras_E");
   E235 : Short_Integer; pragma Import (Ada, E235, "lights_E");
   E239 : Short_Integer; pragma Import (Ada, E239, "materials_E");
   E207 : Short_Integer; pragma Import (Ada, E207, "objects_E");
   E215 : Short_Integer; pragma Import (Ada, E215, "scenes_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "tone_maps_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "tracers_E");
   E209 : Short_Integer; pragma Import (Ada, E209, "utilities_E");
   E243 : Short_Integer; pragma Import (Ada, E243, "shadepoints_E");
   E245 : Short_Integer; pragma Import (Ada, E245, "cameras__pinhole_cameras_E");
   E247 : Short_Integer; pragma Import (Ada, E247, "lights__directionals_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "lights__points_E");
   E252 : Short_Integer; pragma Import (Ada, E252, "materials__lambertian_E");
   E254 : Short_Integer; pragma Import (Ada, E254, "materials__phong_E");
   E256 : Short_Integer; pragma Import (Ada, E256, "materials__reflective_E");
   E258 : Short_Integer; pragma Import (Ada, E258, "materials__transparent_E");
   E262 : Short_Integer; pragma Import (Ada, E262, "objects__csg_objects_E");
   E272 : Short_Integer; pragma Import (Ada, E272, "objects__triangles_E");
   E264 : Short_Integer; pragma Import (Ada, E264, "objects__unit_cones_E");
   E266 : Short_Integer; pragma Import (Ada, E266, "objects__unit_cubes_E");
   E268 : Short_Integer; pragma Import (Ada, E268, "objects__unit_cylinders_E");
   E270 : Short_Integer; pragma Import (Ada, E270, "objects__unit_spheres_E");
   E276 : Short_Integer; pragma Import (Ada, E276, "core_tests_E");
   E260 : Short_Integer; pragma Import (Ada, E260, "objects__compounds_E");
   E274 : Short_Integer; pragma Import (Ada, E274, "samplers__unitsquares_E");
   E005 : Short_Integer; pragma Import (Ada, E005, "build_functions_E");
   E278 : Short_Integer; pragma Import (Ada, E278, "illustrations_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E274 := E274 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "samplers__unitsquares__finalize_spec");
      begin
         F1;
      end;
      E270 := E270 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "objects__unit_spheres__finalize_spec");
      begin
         F2;
      end;
      E268 := E268 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "objects__unit_cylinders__finalize_spec");
      begin
         F3;
      end;
      E266 := E266 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "objects__unit_cubes__finalize_spec");
      begin
         F4;
      end;
      E264 := E264 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "objects__unit_cones__finalize_spec");
      begin
         F5;
      end;
      E272 := E272 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "objects__triangles__finalize_spec");
      begin
         F6;
      end;
      E262 := E262 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "objects__csg_objects__finalize_spec");
      begin
         F7;
      end;
      E258 := E258 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "materials__transparent__finalize_spec");
      begin
         F8;
      end;
      E256 := E256 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "materials__reflective__finalize_spec");
      begin
         F9;
      end;
      E254 := E254 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "materials__phong__finalize_spec");
      begin
         F10;
      end;
      E252 := E252 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "materials__lambertian__finalize_spec");
      begin
         F11;
      end;
      E250 := E250 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "lights__points__finalize_spec");
      begin
         F12;
      end;
      E247 := E247 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "lights__directionals__finalize_spec");
      begin
         F13;
      end;
      E245 := E245 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "cameras__pinhole_cameras__finalize_spec");
      begin
         F14;
      end;
      E237 := E237 - 1;
      E207 := E207 - 1;
      E239 := E239 - 1;
      E148 := E148 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "objects__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "materials__finalize_spec");
      begin
         F16;
      end;
      E235 := E235 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "lights__finalize_spec");
      begin
         F17;
      end;
      declare
         procedure F18;
         pragma Import (Ada, F18, "cameras__finalize_spec");
      begin
         F18;
      end;
      declare
         procedure F19;
         pragma Import (Ada, F19, "samplers__finalize_spec");
      begin
         F19;
      end;
      E150 := E150 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "system__pool_global__finalize_spec");
      begin
         F20;
      end;
      E101 := E101 - 1;
      declare
         procedure F21;
         pragma Import (Ada, F21, "ada__text_io__finalize_spec");
      begin
         F21;
      end;
      E219 := E219 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "ada__strings__unbounded__finalize_spec");
      begin
         F22;
      end;
      E156 := E156 - 1;
      declare
         procedure F23;
         pragma Import (Ada, F23, "system__storage_pools__subpools__finalize_spec");
      begin
         F23;
      end;
      E158 := E158 - 1;
      declare
         procedure F24;
         pragma Import (Ada, F24, "system__finalization_masters__finalize_spec");
      begin
         F24;
      end;
      declare
         procedure F25;
         pragma Import (Ada, F25, "system__file_io__finalize_body");
      begin
         E113 := E113 - 1;
         F25;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E027 := E027 + 1;
      Ada.Containers'Elab_Spec;
      E042 := E042 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E072 := E072 + 1;
      Ada.Strings'Elab_Spec;
      E057 := E057 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E059 := E059 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E063 := E063 + 1;
      Interfaces.C'Elab_Spec;
      E047 := E047 + 1;
      System.Exceptions'Elab_Spec;
      E029 := E029 + 1;
      System.Object_Reader'Elab_Spec;
      E083 := E083 + 1;
      System.Dwarf_Lines'Elab_Spec;
      E052 := E052 + 1;
      System.Os_Lib'Elab_Body;
      E077 := E077 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E023 := E023 + 1;
      E015 := E015 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E041 := E041 + 1;
      E010 := E010 + 1;
      Ada.Numerics'Elab_Spec;
      E136 := E136 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E105 := E105 + 1;
      Ada.Streams'Elab_Spec;
      E103 := E103 + 1;
      System.File_Control_Block'Elab_Spec;
      E117 := E117 + 1;
      System.Finalization_Root'Elab_Spec;
      E116 := E116 + 1;
      Ada.Finalization'Elab_Spec;
      E114 := E114 + 1;
      System.File_Io'Elab_Body;
      E113 := E113 + 1;
      System.Storage_Pools'Elab_Spec;
      E154 := E154 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E158 := E158 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E156 := E156 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E219 := E219 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E144 := E144 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E213 := E213 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E101 := E101 + 1;
      System.Pool_Global'Elab_Spec;
      E150 := E150 + 1;
      System.Random_Seed'Elab_Body;
      E142 := E142 + 1;
      E174 := E174 + 1;
      E217 := E217 + 1;
      E205 := E205 + 1;
      Hitpoints'Elab_Spec;
      E166 := E166 + 1;
      Samplers'Elab_Spec;
      Cameras'Elab_Spec;
      Lights'Elab_Spec;
      Lights'Elab_Body;
      E235 := E235 + 1;
      Materials'Elab_Spec;
      Objects'Elab_Spec;
      Scenes'Elab_Spec;
      E209 := E209 + 1;
      Cameras'Elab_Body;
      E148 := E148 + 1;
      Materials'Elab_Body;
      E239 := E239 + 1;
      Objects'Elab_Body;
      E207 := E207 + 1;
      E215 := E215 + 1;
      E243 := E243 + 1;
      Samplers'Elab_Body;
      E237 := E237 + 1;
      E233 := E233 + 1;
      E231 := E231 + 1;
      E164 := E164 + 1;
      Cameras.Pinhole_Cameras'Elab_Spec;
      Cameras.Pinhole_Cameras'Elab_Body;
      E245 := E245 + 1;
      Lights.Directionals'Elab_Spec;
      Lights.Directionals'Elab_Body;
      E247 := E247 + 1;
      Lights.Points'Elab_Spec;
      Lights.Points'Elab_Body;
      E250 := E250 + 1;
      Materials.Lambertian'Elab_Spec;
      Materials.Lambertian'Elab_Body;
      E252 := E252 + 1;
      Materials.Phong'Elab_Spec;
      Materials.Phong'Elab_Body;
      E254 := E254 + 1;
      Materials.Reflective'Elab_Spec;
      Materials.Reflective'Elab_Body;
      E256 := E256 + 1;
      Materials.Transparent'Elab_Spec;
      Materials.Transparent'Elab_Body;
      E258 := E258 + 1;
      Objects.Csg_Objects'Elab_Spec;
      Objects.Csg_Objects'Elab_Body;
      E262 := E262 + 1;
      Objects.Triangles'Elab_Spec;
      Objects.Triangles'Elab_Body;
      E272 := E272 + 1;
      Objects.Unit_Cones'Elab_Spec;
      Objects.Unit_Cones'Elab_Body;
      E264 := E264 + 1;
      Objects.Unit_Cubes'Elab_Spec;
      Objects.Unit_Cubes'Elab_Body;
      E266 := E266 + 1;
      Objects.Unit_Cylinders'Elab_Spec;
      Objects.Unit_Cylinders'Elab_Body;
      E268 := E268 + 1;
      Objects.Unit_Spheres'Elab_Spec;
      Objects.Unit_Spheres'Elab_Body;
      E270 := E270 + 1;
      E276 := E276 + 1;
      E260 := E260 + 1;
      Samplers.Unitsquares'Elab_Spec;
      Samplers.Unitsquares'Elab_Body;
      E274 := E274 + 1;
      E005 := E005 + 1;
      E278 := E278 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_m_dart");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /home/mlaban/Dev/M_Dart/obj/core_types.o
   --   /home/mlaban/Dev/M_Dart/obj/large_float_functions.o
   --   /home/mlaban/Dev/M_Dart/obj/normal_float_functions.o
   --   /home/mlaban/Dev/M_Dart/obj/small_float_functions.o
   --   /home/mlaban/Dev/M_Dart/obj/linear_math.o
   --   /home/mlaban/Dev/M_Dart/obj/hitpoints.o
   --   /home/mlaban/Dev/M_Dart/obj/lights.o
   --   /home/mlaban/Dev/M_Dart/obj/utilities.o
   --   /home/mlaban/Dev/M_Dart/obj/cameras.o
   --   /home/mlaban/Dev/M_Dart/obj/materials.o
   --   /home/mlaban/Dev/M_Dart/obj/objects.o
   --   /home/mlaban/Dev/M_Dart/obj/scenes.o
   --   /home/mlaban/Dev/M_Dart/obj/shadepoints.o
   --   /home/mlaban/Dev/M_Dart/obj/samplers.o
   --   /home/mlaban/Dev/M_Dart/obj/spectra.o
   --   /home/mlaban/Dev/M_Dart/obj/tone_maps.o
   --   /home/mlaban/Dev/M_Dart/obj/tracers.o
   --   /home/mlaban/Dev/M_Dart/obj/cameras-pinhole_cameras.o
   --   /home/mlaban/Dev/M_Dart/obj/lights-directionals.o
   --   /home/mlaban/Dev/M_Dart/obj/lights-points.o
   --   /home/mlaban/Dev/M_Dart/obj/materials-lambertian.o
   --   /home/mlaban/Dev/M_Dart/obj/materials-phong.o
   --   /home/mlaban/Dev/M_Dart/obj/materials-reflective.o
   --   /home/mlaban/Dev/M_Dart/obj/materials-transparent.o
   --   /home/mlaban/Dev/M_Dart/obj/objects-csg_objects.o
   --   /home/mlaban/Dev/M_Dart/obj/objects-triangles.o
   --   /home/mlaban/Dev/M_Dart/obj/objects-unit_cones.o
   --   /home/mlaban/Dev/M_Dart/obj/objects-unit_cubes.o
   --   /home/mlaban/Dev/M_Dart/obj/objects-unit_cylinders.o
   --   /home/mlaban/Dev/M_Dart/obj/objects-unit_spheres.o
   --   /home/mlaban/Dev/M_Dart/obj/core_tests.o
   --   /home/mlaban/Dev/M_Dart/obj/objects-compounds.o
   --   /home/mlaban/Dev/M_Dart/obj/samplers-unitsquares.o
   --   /home/mlaban/Dev/M_Dart/obj/build_functions.o
   --   /home/mlaban/Dev/M_Dart/obj/illustrations.o
   --   /home/mlaban/Dev/M_Dart/obj/m_dart.o
   --   -L/home/mlaban/Dev/M_Dart/obj/
   --   -L/home/mlaban/Dev/M_Dart/obj/
   --   -L/home/mlaban/Dev/M_GraphiX/lib/
   --   -L/usr/lib/gcc/x86_64-linux-gnu/10/adalib/
   --   -shared
   --   -lgnat-10
   --   -ldl
--  END Object file/option list   

end ada_main;
