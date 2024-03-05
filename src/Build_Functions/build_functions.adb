with Core_Types;                use Core_Types;
with Linear_Math;               use Linear_Math;
with Scenes;                    use Scenes;
with Samplers;                  use Samplers;
with Samplers.UnitSquares;      use Samplers.UnitSquares;
with Objects;                   use Objects;
with Objects.CSG_Objects;       use Objects.CSG_Objects;
with Objects.Unit_Cubes;        use Objects.Unit_Cubes;
with Objects.Unit_Spheres;      use Objects.Unit_Spheres;
with Objects.Unit_Cylinders;    use Objects.Unit_Cylinders;
with Objects.Unit_Cones;        use Objects.Unit_Cones;
with Objects.Compounds;         use Objects.Compounds;
with Objects.Triangles;         use Objects.Triangles;
with Materials;                 use Materials;
with Materials.Lambertian;      use Materials.Lambertian;
with Materials.Phong;           use Materials.Phong;
with Materials.Reflective;      use Materials.Reflective;
with Materials.Transparent;     use Materials.Transparent;
with Lights;                    use Lights;
with Lights.Points;             use Lights.Points;
with Lights.Directionals;       use Lights.Directionals;
with Cameras;                   use Cameras;
with Cameras.Pinhole_Cameras;   use Cameras.Pinhole_Cameras;
with Spectra;                   use Spectra;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Small_Float_Functions;     use Small_Float_Functions;
with Normal_Float_Functions;    use Normal_Float_Functions;
with Large_Float_Functions;     use Large_Float_Functions;

--  MinDist distro's
--  for 4 samples: 0.5
--  for 9 samples: 0.36
--  for 16 samples: 0.24
--  for 25 samples: 0.17
--  for 36 samples: 0.15
--  for 49 samples: 0.128
--  for 64 samples: 0.113
--  for 81 samples: 0.096
--  for 100 samples: 0.086
--  for 121 samples: 0.078
--  for 144 samples: 0.073
--  for 169 sample: 0.067
--  for 196 samples: 0.061
--  for 225 samples: 0.057
--  for 256 samples: 0.0535
--  for 1000 samples: 0.026

package body Build_Functions is

   procedure Triangle_Test is
      Cam_Ptr                        : Camera_Ptr;
      Smp_Ptr                        : Sampler_Ptr;
      Obj1                           : Object_Ptr;
      Trn1                           : Matrix_3D;
      Obj_List                       : Object_List;
      Lt_List                        : Light_List;
      Dir_Lt1, Dir_Lt2               : Light_Ptr;
      Mat1_Ptr                       : Material_Ptr;
      Position                       : Point_3D;
      Look_At, Mid                   : Point_3D;
      Up                             : Vector_3D;
      V1, V2, V3                     : Point_3D;
      dpow, pow                      : Integer;
      Amb_Spec, Diff_Spec, Spec_Spec : RGB_Spectrum;
      MaxZ, Col, nSPheres            : Large_Integer := 0;
   begin
      --  Build the world...
      Position := Construct_Point (0.0, 0.0, 50.0);
      Look_At  := Construct_Point (0.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 768, 768, 4.0, 50.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (25, "My Sampler");
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      Amb_Spec := Construct_RGB_Spectrum (1.0, 0.0, 0.0);
      dpow     := 10;
      pow      := 10;
      for Y in -1 .. 1 loop
         for X in -1 .. 1 loop
            V1        := Construct_Point (Large_Float (X) - 0.4, Large_Float (-Y) - 0.4, 0.0);
            V2        := Construct_Point (Large_Float (X) + 0.4, Large_Float (-Y) - 0.4, 0.0);
            V3        := Construct_Point (Large_Float (X), Large_Float (-Y) + 0.4, 0.0);
            Obj1      := Construct_Triangle ("Tri", V1, V2, V3);
            Diff_Spec := Construct_RGB_Spectrum (1.0, 0.0, 0.0);
            Spec_Spec := Construct_RGB_Spectrum (1.0, 1.0, 1.0);
            Mat1_Ptr  := Construct_Phong ("Shiny Red", Amb_Spec, Diff_Spec, Spec_Spec, 0.2, 0.3, 0.5, pow);
            Obj1.Set_Object_Material (Mat1_Ptr);
            Add_Object (Obj1, Obj_List);
            pow := pow + dpow;
         end loop;
         dpow := dpow + 10;
      end loop;

      --  Set the lights
      Set_Ambient_Light (Construct_RGB_Spectrum (0.15, 0.15, 0.15));
      Use_Ambient_Light (True);

      Dir_Lt1 := Construct_Point_Light ("My Point Light 1", 0.5, WHITE_RGB_Spec, Construct_Point (1.0, 0.0, 1.0), True);
      Add_Light (Dir_Lt1, Lt_List);
      Dir_Lt2 := Construct_Point_Light ("My Point Light 2", 0.7, WHITE_RGB_Spec, Construct_Point (0.0, 50.0, 50.0), True);
      Add_Light (Dir_Lt2, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Name ("Triangle_Test");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/Triangle_Test.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;

   end Triangle_Test;

   procedure Triangle_Test2 is
      Cam_Ptr                        : Camera_Ptr;
      Smp_Ptr                        : Sampler_Ptr;
      Obj1                           : Object_Ptr;
      Mtr                            : Matrix_3D;
      Obj_List                       : Object_List;
      Lt_List                        : Light_List;
      Dir_Lt1, Dir_Lt2               : Light_Ptr;
      Mat1_Ptr                       : Material_Ptr;
      Position                       : Point_3D;
      Look_At, Mid                   : Point_3D;
      Up                             : Vector_3D;
      V1, V2, V3                     : Point_3D;
      pow                            : Integer;
      Amb_Spec, Diff_Spec, Spec_Spec : RGB_Spectrum;
      MaxZ, Col, nSPheres            : Large_Integer := 0;
   begin
      --  Build the world...
      Position := Construct_Point (0.0, 0.0, 50.0);
      Look_At  := Construct_Point (0.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 20.0, 50.0);
      Smp_Ptr  := Construct_UnitSquare_Random_Sampler (9, 0.2, "My Sampler");
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      Mtr := Construct_Rotate_Z(0.5);
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/Triangle_Test_05.ppm");

      -- 1st red triangle
      Amb_Spec  := Construct_RGB_Spectrum (1.0, 0.0, 0.0);
      pow       := 10;
      V1        := Construct_Point (-5.0, 5.0, -9.0);
      V2        := Construct_Point (-6.0, 7.0, 9.0);
      V3        := Construct_Point (7.0, 5.0, 0.0);
      Obj1      := Construct_Triangle ("Tri", V1, V2, V3);
      Diff_Spec := Construct_RGB_Spectrum (1.0, 0.0, 0.0);
      Spec_Spec := Construct_RGB_Spectrum (1.0, 1.0, 1.0);
      Mat1_Ptr  := Construct_Phong ("Shiny Red", Amb_Spec, Diff_Spec, Spec_Spec, 0.2, 0.3, 0.5, pow);
      Obj1.Set_Object_Material (Mat1_Ptr);
      Obj1.Transform(Mtr);
      Add_Object (Obj1, Obj_List);

      -- 2nd yellow triangle
      Amb_Spec  := Construct_RGB_Spectrum (1.0, 1.0, 0.0);
      pow       := 10;
      V1        := Construct_Point (3.0, 6.0, -9.0);
      V2        := Construct_Point (5.0, 6.0, 9.0);
      V3        := Construct_Point (0.0, -6.0, 8.0);
      Obj1      := Construct_Triangle ("Tri", V1, V2, V3);
      Diff_Spec := Construct_RGB_Spectrum (1.0, 1.0, 0.0);
      Spec_Spec := Construct_RGB_Spectrum (1.0, 1.0, 1.0);
      Mat1_Ptr  := Construct_Phong ("Shiny Red", Amb_Spec, Diff_Spec, Spec_Spec, 0.2, 0.3, 0.5, pow);
      Obj1.Set_Object_Material (Mat1_Ptr);
      Obj1.Transform(Mtr);
      Add_Object (Obj1, Obj_List);

      -- 3rd green triangle
      Amb_Spec  := Construct_RGB_Spectrum (0.0, 1.0, 0.0);
      pow       := 10;
      V1        := Construct_Point (0.0, -5.0, -8.0);
      V2        := Construct_Point (3.0, -5.0, 8.0);
      V3        := Construct_Point (-5.0, 7.0, 0.0);
      Obj1      := Construct_Triangle ("Tri", V1, V2, V3);
      Diff_Spec := Construct_RGB_Spectrum (0.0, 1.0, 0.0);
      Spec_Spec := Construct_RGB_Spectrum (1.0, 1.0, 1.0);
      Mat1_Ptr  := Construct_Phong ("Shiny Red", Amb_Spec, Diff_Spec, Spec_Spec, 0.2, 0.3, 0.5, pow);
      Obj1.Set_Object_Material (Mat1_Ptr);
      Obj1.Transform(Mtr);
      Add_Object (Obj1, Obj_List);

      --  Set the lights
      Set_Ambient_Light (Construct_RGB_Spectrum (0.25, 0.25, 0.25));
      Use_Ambient_Light (True);

      Dir_Lt1 := Construct_Point_Light ("My Point Light 1", 0.7, WHITE_RGB_Spec, Construct_Point (-10.0, -10.0, 10.0), True);
      Add_Light (Dir_Lt1, Lt_List);
      Dir_Lt2 := Construct_Point_Light ("My Point Light 2", 0.7, WHITE_RGB_Spec, Construct_Point (100.0, 10.0, 50.0), False);
      Add_Light (Dir_Lt2, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Name ("Triangle_Test");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;

   end Triangle_Test2;

   procedure Transparency_Test2 is
      Cam_Ptr                            : Camera_Ptr;
      Smp_Ptr                            : Sampler_Ptr;
      Tile, Boarder                      : Object_Ptr;
      Trn1                               : Matrix_3D;
      Obj_List                           : Object_List;
      CSG_Obj_List                       : Object_List;
      Lt_List                            : Light_List;
      Lt1, Lt2, Lt3                      : Light_Ptr;
      Black_Tile, White_Tile             : Material_Ptr;
      Boarder_Mat, Glass                 : Material_Ptr;
      Glass_Obj1, Glass_Obj2, Glass_Obj3 : Object_Ptr;
      Position                           : Point_3D;
      Look_At                            : Point_3D;
      Up                                 : Vector_3D;
      Odd, Odd2                          : Boolean := False;
      gX, gZ                             : Large_Float;

   begin
      --  Build the world...
      Position := Construct_Point (70.0, 25.0, 20.0);
      --  Position := Construct_Point(6.8, 2.25, 6.7);
      Look_At := Construct_Point (0.0, 3.0, 0.0);
      Up      := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 15.0, 100.0);
      Smp_Ptr := Construct_UnitSquare_Regular_Sampler (1, "My Sampler");
      Set_Max_Recursion_Level (5);
      Set_Minimum_Weight (0.0);

      White_Tile :=
        Construct_Phong
          ("White Tile", Construct_RGB_Spectrum (0.0, 0.0, 0.0), Construct_RGB_Spectrum (0.3, 0.3, 0.3),
           Construct_RGB_Spectrum (0.99, 0.99, 0.99), 1.0, 1.0, 1.0, 100);

      Black_Tile :=
        Construct_Phong

          ("Black Tile", Construct_RGB_Spectrum (0.1, 0.1, 0.1), Construct_RGB_Spectrum (0.5, 0.3, 0.1),
           Construct_RGB_Spectrum (0.99, 0.99, 0.99), 1.0, 1.0, 1.0, 100);

      Boarder_Mat :=
        Construct_Phong
          ("Boarder", Construct_RGB_Spectrum (0.3, 0.2, 0.1), Construct_RGB_Spectrum (0.5, 0.3, 0.1),
           Construct_RGB_Spectrum (0.7, 0.7, 0.7), 1.0, 1.0, 1.0, 100);

      Glass :=
        Construct_Transparent
          ("Glass", Construct_RGB_Spectrum (0.0, 0.0, 0.0), Construct_RGB_Spectrum (0.99, 0.99, 0.99),
           Construct_RGB_Spectrum (0.99, 0.99, 0.99), Construct_RGB_Spectrum (0.9, 0.99, 0.9),
           Construct_RGB_Spectrum (0.9, 0.99, 0.9), 0.0, 0.0, 0.0, 0.1, 0.9, 100, 1.5);

      --  The ChessBoard...first the tiles
      for Z in -3 .. 4 loop

         if Odd2 then
            Odd  := False;
            Odd2 := False;
         else
            Odd  := True;
            Odd2 := True;
         end if;

         for X in -3 .. 4 loop
            Tile := Construct_Unit_Cube ("Tile" & X'Image & " - " & Z'Image);
            gZ   := 2.0 * Large_Float (Z) - 1.0;
            gX   := 2.0 * Large_Float (X) - 1.0;
            Trn1 := Construct_Translation (gX, 0.0, gZ) * Construct_Scale (1.0, 0.2, 1.0);
            Tile.Transform (Trn1);
            if Odd then
               Tile.Set_Object_Material (White_Tile);
               Odd := False;
            else
               Tile.Set_Object_Material (Black_Tile);
               Odd := True;
            end if;
            Add_Object (Tile, Obj_List);

         end loop;

      end loop;

      --  Then some decent boarders
      Boarder :=
        Construct_Positioned_Cylinder ("Boarder1", Construct_Point (-8.0, 0.0, -8.0), Construct_Point (-8.0, 0.0, 8.0), 0.25);
      Boarder.Set_Object_Material (Boarder_Mat);
      Add_Object (Boarder, Obj_List);

      Boarder :=
        Construct_Positioned_Cylinder ("Boarder2", Construct_Point (-8.0, 0.0, -8.0), Construct_Point (8.0, 0.0, -8.0), 0.25);
      Boarder.Set_Object_Material (Boarder_Mat);
      Add_Object (Boarder, Obj_List);

      Boarder :=
        Construct_Positioned_Cylinder ("Boarder3", Construct_Point (8.0, 0.0, -8.0), Construct_Point (8.0, 0.0, 8.0), 0.25);
      Boarder.Set_Object_Material (Boarder_Mat);
      Add_Object (Boarder, Obj_List);

      Boarder :=
        Construct_Positioned_Cylinder ("Boarder4", Construct_Point (8.0, 0.0, 8.0), Construct_Point (-8.0, 0.0, 8.0), 0.25);
      Boarder.Set_Object_Material (Boarder_Mat);
      Add_Object (Boarder, Obj_List);

      Boarder := Construct_Positioned_Sphere ("Sphere1", Construct_Point (-8.0, 0.0, -8.0), 0.35);
      Boarder.Set_Object_Material (Boarder_Mat);
      Add_Object (Boarder, Obj_List);

      Boarder := Construct_Positioned_Sphere ("Sphere2", Construct_Point (-8.0, 0.0, 8.0), 0.35);
      Boarder.Set_Object_Material (Boarder_Mat);
      Add_Object (Boarder, Obj_List);

      Boarder := Construct_Positioned_Sphere ("Sphere3", Construct_Point (8.0, 0.0, -8.0), 0.35);
      Boarder.Set_Object_Material (Boarder_Mat);
      Add_Object (Boarder, Obj_List);

      Boarder := Construct_Positioned_Sphere ("Sphere4", Construct_Point (8.0, 0.0, 8.0), 0.35);
      Boarder.Set_Object_Material (Boarder_Mat);
      Add_Object (Boarder, Obj_List);

      Glass_Obj1 := Construct_Unit_Cube ("Cube 1");
      Trn1       := Construct_Translation (0.0, 1.81, -2.0) * Construct_Rotate_Y (Ray_PI_div4) * Construct_Scale (3.0, 1.5, 0.2);
      Glass_Obj1.Transform (Trn1);
      Glass_Obj2 := Construct_Unit_Cube ("Cube 1");
      Trn1       := Construct_Translation (0.0, 1.71, 0.0) * Construct_Rotate_Y (Ray_PI_div4) * Construct_Scale (2.7, 1.3, 1.2);
      Glass_Obj2.Transform (Trn1);
      Glass_Obj3 := Construct_Unit_Cube ("Cube 1");
      Trn1       := Construct_Translation (0.0, 1.41, 2.0) * Construct_Rotate_Y (Ray_PI_div4) * Construct_Scale (2.3, 1.1, 0.2);
      Glass_Obj3.Transform (Trn1);

   --  Glass_Obj1 := Construct_Positioned_Sphere ("Glass Sphere 1", Construct_Point (0.0, 3.5, 0.0), 3.0); Glass_Obj2
   --  := Construct_Positioned_Sphere ("Glass Sphere 2", Construct_Point (1.0, 3.5, 0.0), 2.9); Glass_Obj1 :=
   --  Construct_Positioned_Cylinder ("Glass Object 1", Construct_Point (0.0, 3.0, 0.0), Construct_Point (0.0, 4.0, 0.0), 3.0);
   --  Glass_Obj2 := Construct_Positioned_Cylinder ("Glass Object 2", Construct_Point (0.0, 3.1, 0.0), Construct_Point (0.0, 3.9,
   --  0.0), 2.9);

      --  Glass_Obj3 := CSG_Intersection (Glass_Obj1, Glass_Obj2, CSG_Obj_List); Glass_Obj3.Set_Object_Material (Glass);
      Glass_Obj1.Set_Object_Material (Glass);
      Glass_Obj2.Set_Object_Material (Glass);
      Glass_Obj3.Set_Object_Material (Glass);
      -- Add_Object (Glass_Obj1, Obj_List);
      Add_Object (Glass_Obj2, Obj_List);
      -- Add_Object (Glass_Obj3, Obj_List);

      --  Add it all up
      Set_CSG_Object_List (CSG_Obj_List);
      --  Set_CSG_Tree (Glass_Obj3);

      --  Lights
      Lt1 := Construct_Point_Light ("Point Light", 0.35, WHITE_RGB_Spec, Construct_Point (0.0, 10.0, 1.0), False);
      Add_Light (Lt1, Lt_List);
      Lt2 := Construct_Point_Light ("Point Light", 0.35, WHITE_RGB_Spec, Construct_Point (20.0, 5.0, 20.0), False);
      Add_Light (Lt2, Lt_List);
      Lt3 := Construct_Point_Light ("Point Light", 0.35, WHITE_RGB_Spec, Construct_Point (20.0, 15.0, 8.0), False);
      Add_Light (Lt3, Lt_List);
      Set_Ambient_Light (Construct_RGB_Spectrum (0.2, 0.2, 0.2));

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.6, 0.6, 0.8));

      Set_Name ("Transparency Test");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/transparency_test2-weight-0.00000001.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;

   end Transparency_Test2;

   procedure Transparency_Test is
      Cam_Ptr                      : Camera_Ptr;
      Smp_Ptr                      : Sampler_Ptr;
      Obj1, Obj2, Obj3, Obj4, Flr  : Object_Ptr;
      Trn1                         : Matrix_3D;
      Obj_List                     : Object_List;
      Lt_List                      : Light_List;
      Dir_Lt1, Dir_Lt2             : Light_Ptr;
      Mat1_Ptr, Mat2_Ptr, Mat3_Ptr : Material_Ptr;
      Mat4_Ptr, Mat5_Ptr           : Material_Ptr;
      Position                     : Point_3D;
      Look_At, Mid                 : Point_3D;
      Up                           : Vector_3D;
      MaxZ, Col, nSPheres          : Large_Integer := 0;
   begin
      --  Build the world...
      Position := Construct_Point (0.0, 500.0, 1.0);
      Look_At  := Construct_Point (0.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 5.0, 100.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (1, "My Sampler");
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      Mat1_Ptr := Construct_Phong ("Shiny Red", RED_RGB_Spec, RED_RGB_Spec, WHITE_RGB_Spec, 0.2, 0.3, 0.5, 100);
      Mat2_Ptr := Construct_Phong ("Shiny Red", YELLOW_RGB_Spec, YELLOW_RGB_Spec, WHITE_RGB_Spec, 0.2, 0.3, 0.5, 100);
      Mat3_Ptr := Construct_Phong ("Shiny Red", GREEN_RGB_Spec, GREEN_RGB_Spec, WHITE_RGB_Spec, 0.2, 0.3, 0.5, 100);

      Mat4_Ptr :=
        Construct_Reflective
          ("Mirror", BLACK_RGB_Spec, Construct_RGB_Spectrum (0.2, 0.2, 0.2), Construct_RGB_Spectrum (0.8, 0.8, 0.8),
           Construct_RGB_Spectrum (0.2, 0.2, 0.2), 0.2, 0.3, 0.5, 0.5, 200);

      Mat5_Ptr :=
        Construct_Transparent
          ("Glass", WHITE_RGB_Spec, WHITE_RGB_Spec, WHITE_RGB_Spec, WHITE_RGB_Spec, WHITE_RGB_Spec, 0.0, 0.0, 0.0, 0.1, 0.9, 200,
           1.4);

      Obj1 := Construct_Positioned_Cone ("Cone1", Construct_Point (0.0, 0.0, 0.0), Construct_Point (0.0, 3.0, 0.0), 1.0);
      Obj1.Set_Object_Material (Mat5_Ptr);
      Add_Object (Obj1, Obj_List);

      Obj2 := Construct_Positioned_Cone ("Cone2", Construct_Point (-2.0, 0.0, 2.0), Construct_Point (-2.0, 3.0, 2.0), 1.0);
      Obj2.Set_Object_Material (Mat1_Ptr);
      Add_Object (Obj2, Obj_List);

      Obj3 := Construct_Positioned_Cone ("Cone3", Construct_Point (2.0, 0.0, 2.0), Construct_Point (2.0, 3.0, 2.0), 1.0);
      Obj3.Set_Object_Material (Mat1_Ptr);
      Add_Object (Obj3, Obj_List);

      Obj4 := Construct_Positioned_Sphere ("Trans", Construct_Point (0.0, 1.5, 4.0), 1.0);
      Obj4.Set_Object_Material (Mat1_Ptr);
      Add_Object (Obj4, Obj_List);

      Flr  := Construct_Unit_Cube ("Floor");
      Trn1 := Construct_Translation (0.0, -1.0, 0.0) * Construct_Scale (100.0, 1.0, 100.0);
      Flr.Set_Object_Material (Mat4_Ptr);
      Flr.Transform (Trn1);
      Add_Object (Flr, Obj_List);

      --  Set the lights
      Dir_Lt1 := Construct_Point_Light ("My Point Light 1", 0.5, WHITE_RGB_Spec, Construct_Point (500.0, 500.0, 500.0), True);
      Add_Light (Dir_Lt1, Lt_List);

      Dir_Lt2 := Construct_Point_Light ("My Point Light 2", 0.5, WHITE_RGB_Spec, Construct_Point (-500.0, 500.0, 500.0), True);
      Add_Light (Dir_Lt2, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Name ("Transparency Test Scene");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/transparency_test.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;

   end Transparency_Test;

   procedure Different_Phong_Spheres1 is
      Cam_Ptr                        : Camera_Ptr;
      Smp_Ptr                        : Sampler_Ptr;
      Obj1                           : Object_Ptr;
      Trn1                           : Matrix_3D;
      Obj_List                       : Object_List;
      Lt_List                        : Light_List;
      Dir_Lt1, Dir_Lt2               : Light_Ptr;
      Mat1_Ptr                       : Material_Ptr;
      Position                       : Point_3D;
      Look_At, Mid                   : Point_3D;
      Up                             : Vector_3D;
      SphPos                         : Point_3D;
      dpow, pow                      : Integer;
      Amb_Spec, Diff_Spec, Spec_Spec : RGB_Spectrum;
      MaxZ, Col, nSPheres            : Large_Integer := 0;
   begin
      --  Build the world...
      Position := Construct_Point (0.0, 0.0, 50.0);
      Look_At  := Construct_Point (0.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 4.0, 50.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (25, "My Sampler");
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      Amb_Spec := Construct_RGB_Spectrum (1.0, 0.0, 0.0);
      dpow     := 10;
      pow      := 2;
      for Y in -1 .. 1 loop
         for X in -1 .. 1 loop
            SphPos    := Construct_Point (Large_Float (X), Large_Float (-Y), 0.0);
            Obj1      := Construct_Positioned_Sphere ("Sphere", SphPos, 0.4);
            Diff_Spec := Construct_RGB_Spectrum (1.0, 0.0, 0.0);
            Spec_Spec := Construct_RGB_Spectrum (1.0, 1.0, 1.0);
            Mat1_Ptr  := Construct_Phong ("Shiny Red", Amb_Spec, Diff_Spec, Spec_Spec, 0.2, 0.3, 0.5, pow);
            Obj1.Set_Object_Material (Mat1_Ptr);
            Add_Object (Obj1, Obj_List);
            pow := pow + dpow;
         end loop;
         dpow := dpow + 10;
      end loop;

      --  Set the lights
      Set_Ambient_Light (Construct_RGB_Spectrum (0.15, 0.15, 0.15));
      Use_Ambient_Light (True);

      Dir_Lt1 := Construct_Point_Light ("My Point Light 1", 0.5, WHITE_RGB_Spec, Construct_Point (5.0, 0.0, 5.0), True);
      Add_Light (Dir_Lt1, Lt_List);
      Dir_Lt2 := Construct_Point_Light ("My Point Light 2", 0.7, WHITE_RGB_Spec, Construct_Point (0.0, 50.0, 50.0), True);
      Add_Light (Dir_Lt2, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Name ("Phong_Balls_Power");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/Phong_Balls_Power.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;

   end Different_Phong_Spheres1;

   procedure Different_Phong_Spheres2 is
      Cam_Ptr                        : Camera_Ptr;
      Smp_Ptr                        : Sampler_Ptr;
      Obj1                           : Object_Ptr;
      Trn1                           : Matrix_3D;
      Obj_List                       : Object_List;
      Lt_List                        : Light_List;
      Dir_Lt1, Dir_Lt2               : Light_Ptr;
      Mat1_Ptr                       : Material_Ptr;
      Position                       : Point_3D;
      Look_At, Mid                   : Point_3D;
      Up                             : Vector_3D;
      SphPos                         : Point_3D;
      dKd, Kd                        : Small_Float;
      Amb_Spec, Diff_Spec, Spec_Spec : RGB_Spectrum;
      MaxZ, Col, nSPheres            : Large_Integer := 0;
   begin
      --  Build the world...
      Position := Construct_Point (0.0, 0.0, 50.0);
      Look_At  := Construct_Point (0.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 4.0, 50.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (25, "My Sampler");
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      Amb_Spec := Construct_RGB_Spectrum (1.0, 0.0, 0.0);

      dKd := 1.0 / 8.0;
      Kd  := 0.0;
      for Y in -1 .. 1 loop
         for X in -1 .. 1 loop
            SphPos    := Construct_Point (Large_Float (X), Large_Float (-Y), 0.0);
            Obj1      := Construct_Positioned_Sphere ("Sphere", SphPos, 0.4);
            Diff_Spec := Construct_RGB_Spectrum (Kd, 0.0, 0.0);
            Spec_Spec := Construct_RGB_Spectrum (1.0 - Kd, 1.0 - Kd, 1.0 - Kd);
            Mat1_Ptr  := Construct_Phong ("Shiny Red", Amb_Spec, Diff_Spec, Spec_Spec, 0.2, 0.3, 0.5, 100);
            Obj1.Set_Object_Material (Mat1_Ptr);
            Add_Object (Obj1, Obj_List);
            Kd := Kd + dKd;
         end loop;
      end loop;

      --  Set the lights
      Set_Ambient_Light (Construct_RGB_Spectrum (0.15, 0.15, 0.15));
      -- Use_Ambient_Light(False);

      Dir_Lt1 := Construct_Point_Light ("My Point Light 1", 0.5, WHITE_RGB_Spec, Construct_Point (50.0, 0.0, 50.0), True);
      Add_Light (Dir_Lt1, Lt_List);
      Dir_Lt2 := Construct_Point_Light ("My Point Light 2", 0.8, WHITE_RGB_Spec, Construct_Point (0.0, 50.0, 50.0), True);
      Add_Light (Dir_Lt2, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Name ("Phong_Balls_Energy");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/Phong_Balls_Energy.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;

   end Different_Phong_Spheres2;

   procedure Different_Phong_Spheres3 is
      Cam_Ptr                        : Camera_Ptr;
      Smp_Ptr                        : Sampler_Ptr;
      Obj1                           : Object_Ptr;
      Trn1                           : Matrix_3D;
      Obj_List                       : Object_List;
      Lt_List                        : Light_List;
      Dir_Lt1, Dir_Lt2               : Light_Ptr;
      Mat1_Ptr                       : Material_Ptr;
      Position                       : Point_3D;
      Look_At, Mid                   : Point_3D;
      Up                             : Vector_3D;
      SphPos                         : Point_3D;
      dKd, Kd                        : Small_Float;
      Amb_Spec, Diff_Spec, Spec_Spec : RGB_Spectrum;
      MaxZ, Col, nSPheres            : Large_Integer := 0;
   begin
      --  Build the world...
      Position := Construct_Point (0.0, 0.0, 50.0);
      Look_At  := Construct_Point (0.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 4.0, 50.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (25, "My Sampler");
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      Amb_Spec := Construct_RGB_Spectrum (1.0, 0.0, 0.0);

      dKd := 1.0 / 8.0;
      Kd  := 0.0;
      for Y in -1 .. 1 loop
         for X in -1 .. 1 loop
            SphPos    := Construct_Point (Large_Float (X), Large_Float (-Y), 0.0);
            Obj1      := Construct_Positioned_Sphere ("Sphere", SphPos, 0.4);
            Diff_Spec := Construct_RGB_Spectrum (Kd, 0.0, 0.0);
            Mat1_Ptr  := Construct_Phong ("Shiny Red", Amb_Spec, Diff_Spec, WHITE_RGB_Spec, 0.2, 0.3, 0.5, 100);
            Obj1.Set_Object_Material (Mat1_Ptr);
            Add_Object (Obj1, Obj_List);
            Kd := Kd + dKd;
         end loop;
      end loop;

      --  Set the lights
      Set_Ambient_Light (Construct_RGB_Spectrum (0.15, 0.15, 0.15));
      -- Use_Ambient_Light(False);

      Dir_Lt1 := Construct_Point_Light ("My Point Light 1", 0.5, WHITE_RGB_Spec, Construct_Point (50.0, 0.0, 50.0), True);
      Add_Light (Dir_Lt1, Lt_List);
      Dir_Lt2 := Construct_Point_Light ("My Point Light 2", 0.8, WHITE_RGB_Spec, Construct_Point (0.0, 50.0, 50.0), True);
      Add_Light (Dir_Lt2, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Name ("Phong_Balls_White");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/Phong_Balls_White.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;

   end Different_Phong_Spheres3;

   procedure Different_Phong_Spheres4 is
      Cam_Ptr                        : Camera_Ptr;
      Smp_Ptr                        : Sampler_Ptr;
      Obj1                           : Object_Ptr;
      Trn1                           : Matrix_3D;
      Obj_List                       : Object_List;
      Lt_List                        : Light_List;
      Dir_Lt1, Dir_Lt2               : Light_Ptr;
      Mat1_Ptr                       : Material_Ptr;
      Position                       : Point_3D;
      Look_At, Mid                   : Point_3D;
      Up                             : Vector_3D;
      SphPos                         : Point_3D;
      dKd, Kd                        : Small_Float;
      Amb_Spec, Diff_Spec, Spec_Spec : RGB_Spectrum;
      MaxZ, Col, nSPheres            : Large_Integer := 0;
   begin
      --  Build the world...
      Position := Construct_Point (0.0, 0.0, 50.0);
      Look_At  := Construct_Point (0.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 4.0, 50.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (25, "My Sampler");
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      Amb_Spec := Construct_RGB_Spectrum (1.0, 0.0, 0.0);

      dKd := 1.0 / 8.0;
      Kd  := 0.0;
      for Y in -1 .. 1 loop
         for X in -1 .. 1 loop
            SphPos    := Construct_Point (Large_Float (X), Large_Float (-Y), 0.0);
            Obj1      := Construct_Positioned_Sphere ("Sphere", SphPos, 0.4);
            Diff_Spec := Construct_RGB_Spectrum (1.0, 0.0, 0.0);
            Mat1_Ptr  := Construct_Phong ("Shiny Red", Amb_Spec, Diff_Spec, WHITE_RGB_Spec, 0.2, Kd, 1.0 - Kd, 100);
            Obj1.Set_Object_Material (Mat1_Ptr);
            Add_Object (Obj1, Obj_List);
            Kd := Kd + dKd;
         end loop;
      end loop;

      --  Set the lights
      Set_Ambient_Light (Construct_RGB_Spectrum (0.15, 0.15, 0.15));
      -- Use_Ambient_Light(False);

      Dir_Lt1 := Construct_Point_Light ("My Point Light 1", 0.5, WHITE_RGB_Spec, Construct_Point (50.0, 0.0, 50.0), True);
      Add_Light (Dir_Lt1, Lt_List);
      Dir_Lt2 := Construct_Point_Light ("My Point Light 2", 0.8, WHITE_RGB_Spec, Construct_Point (0.0, 50.0, 50.0), True);
      Add_Light (Dir_Lt2, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Name ("Phong_Balls_Coeffs");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/Phong_Balls_Coeffs.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;

   end Different_Phong_Spheres4;

   procedure Unclipped_Spheres is
      Cam_Ptr          : Camera_Ptr;
      Smp_Ptr          : Sampler_Ptr;
      Sphere           : Object_Ptr;
      Trn              : Matrix_3D;
      Obj_List         : Object_List;
      Lt_List          : Light_List;
      Dir_Lt1, Dir_Lt2 : Light_Ptr;
      Mat_Ptr          : Material_Ptr;
      Position         : Point_3D;
      Look_At          : Point_3D;
      Up               : Vector_3D;
   begin
      --  Build the world...
      Position := Construct_Point (15.0, 15.0, 20.0);
      Look_At  := Construct_Point (0.0, 1.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 800, 800, 5.0, 25.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (1, "My Sampler");
      Smp_Ptr.Initialize;

      Sphere := Construct_Unit_Sphere ("Sphere 1");
      Add_Object (Sphere, Obj_List);
      Trn     := Construct_Translation (-2.0, 2.0, 0.0);
      Mat_Ptr := Construct_Lambertian ("White", WHITE_RGB_Spec, WHITE_RGB_Spec);
      Sphere.Set_Object_Material (Mat_Ptr);
      Sphere.Transform (Trn);

      Sphere := Construct_Unit_Sphere ("Sphere 2");
      Add_Object (Sphere, Obj_List);
      Trn     := Construct_Translation (0.0, 2.0, 0.0);
      Mat_Ptr := Construct_Lambertian ("Red", RED_RGB_Spec, RED_RGB_Spec);
      Sphere.Set_Object_Material (Mat_Ptr);
      Sphere.Transform (Trn);

      Sphere := Construct_Unit_Sphere ("Sphere 3");
      Add_Object (Sphere, Obj_List);
      Trn     := Construct_Translation (2.0, 2.0, 0.0);
      Mat_Ptr := Construct_Lambertian ("Green", GREEN_RGB_Spec, GREEN_RGB_Spec);
      Sphere.Set_Object_Material (Mat_Ptr);
      Sphere.Transform (Trn);

      Sphere := Construct_Unit_Sphere ("Sphere 4");
      Add_Object (Sphere, Obj_List);
      Trn     := Construct_Translation (-2.0, 0.0, 0.0);
      Mat_Ptr := Construct_Lambertian ("Red", BLUE_RGB_Spec, BLUE_RGB_Spec);
      Sphere.Set_Object_Material (Mat_Ptr);
      Sphere.Transform (Trn);

      Sphere := Construct_Unit_Sphere ("Sphere 5");
      Add_Object (Sphere, Obj_List);
      Trn     := Construct_Translation (0.0, 0.0, 0.0);
      Mat_Ptr := Construct_Lambertian ("Red", YELLOW_RGB_Spec, YELLOW_RGB_Spec);
      Sphere.Set_Object_Material (Mat_Ptr);
      Sphere.Transform (Trn);

      Sphere := Construct_Unit_Sphere ("Sphere 6");
      Add_Object (Sphere, Obj_List);
      Trn     := Construct_Translation (2.0, 0.0, 0.0);
      Mat_Ptr := Construct_Lambertian ("Red", CYAN_RGB_Spec, CYAN_RGB_Spec);
      Sphere.Set_Object_Material (Mat_Ptr);
      Sphere.Transform (Trn);

      --  Set the lights
      Dir_Lt1 :=
        Construct_Directional_Light ("My Direct Light 1", 0.8, WHITE_RGB_Spec, Construct_Vector (-1.0, -0.5, -1.0), False);
      Add_Light (Dir_Lt1, Lt_List);
      Dir_Lt2 :=
        Construct_Directional_Light ("My Direct Light 2", 0.8, WHITE_RGB_Spec, Construct_Vector (-1.0, -0.5, -1.0), False);
      Add_Light (Dir_Lt2, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.5, 0.5, 0.5));
      Set_Name ("Unclipped Spheres");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/unclipped_spheres.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;
   end Unclipped_Spheres;

   procedure Sheared_Cubes is
      Cam_Ptr  : Camera_Ptr;
      Smp_Ptr  : Sampler_Ptr;
      Cube     : Object_Ptr;
      Trn      : Matrix_3D;
      Obj_List : Object_List;
      Lt_List  : Light_List;
      Dir_Lt   : Light_Ptr;
      Mat_Ptr  : Material_Ptr;
      Position : Point_3D;
      Look_At  : Point_3D;
      Up       : Vector_3D;
   begin
      --  Build the world...
      Position := Construct_Point (15.0, 15.0, 20.0);
      Look_At  := Construct_Point (0.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Basic_Camera (Position, Look_At, Up, 512, 512, 15.0, 20.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (1, "My Sampler");
      Smp_Ptr.Initialize;

      Mat_Ptr := Construct_Lambertian ("White", WHITE_RGB_Spec, WHITE_RGB_Spec);

      Cube := Construct_Unit_Cube ("Cube 1");
      Add_Object (Cube, Obj_List);
      Trn :=
        Construct_Translation (-4.0, 2.0, 0.0) * Construct_Shear (0.3, 0.0, 0.0, 0.0, 0.0, 0.0) * Construct_Scale (1.0, 1.0, 2.0);
      Cube.Set_Object_Material (Mat_Ptr);
      Cube.Transform (Trn);

      Cube := Construct_Unit_Cube ("Cube 2");
      Add_Object (Cube, Obj_List);
      Trn :=
        Construct_Translation (0.0, 2.0, 0.0) * Construct_Shear (0.0, 0.3, 0.0, 0.0, 0.0, 0.0) * Construct_Scale (1.0, 1.0, 2.0);
      Cube.Set_Object_Material (Mat_Ptr);
      Cube.Transform (Trn);

      Cube := Construct_Unit_Cube ("Cube 3");
      Add_Object (Cube, Obj_List);
      Trn :=
        Construct_Translation (4.0, 2.0, 0.0) * Construct_Shear (0.0, 0.0, 0.3, 0.0, 0.0, 0.0) * Construct_Scale (1.0, 1.0, 2.0);
      Cube.Set_Object_Material (Mat_Ptr);
      Cube.Transform (Trn);

      Cube := Construct_Unit_Cube ("Cube 4");
      Add_Object (Cube, Obj_List);
      Trn :=
        Construct_Translation (-4.0, -2.0, 0.0) * Construct_Shear (0.0, 0.0, 0.0, 0.3, 0.0, 0.0) * Construct_Scale (1.0, 1.0, 2.0);
      Cube.Set_Object_Material (Mat_Ptr);
      Cube.Transform (Trn);

      Cube := Construct_Unit_Cube ("Cube 5");
      Add_Object (Cube, Obj_List);
      Trn :=
        Construct_Translation (0.0, -2.0, 0.0) * Construct_Shear (0.0, 0.0, 0.0, 0.0, 0.3, 0.0) * Construct_Scale (1.0, 1.0, 2.0);
      Cube.Set_Object_Material (Mat_Ptr);
      Cube.Transform (Trn);

      Cube := Construct_Unit_Cube ("Cube 6");
      Add_Object (Cube, Obj_List);
      Trn :=
        Construct_Translation (4.0, -2.0, 0.0) * Construct_Shear (0.0, 0.0, 0.0, 0.0, 0.0, 0.3) * Construct_Scale (1.0, 1.0, 2.0);
      Cube.Set_Object_Material (Mat_Ptr);
      Cube.Transform (Trn);

      --  Set the lights
      Dir_Lt := Construct_Directional_Light ("My Direct Light", 0.5, WHITE_RGB_Spec, Construct_Vector (-1.5, -1.0, -2.5), False);
      Add_Light (Dir_Lt, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.5));
      Set_Name ("Sheared Cubes");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/sheared_cubes.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;
   end Sheared_Cubes;

   procedure Dull_Sphere is
      Cam_Ptr       : Camera_Ptr;
      Smp_Ptr       : Sampler_Ptr;
      Sphere        : Object_Ptr;
      Trn           : Matrix_3D;
      Obj_List      : Object_List;
      Lt_List       : Light_List;
      Lt1, Lt2, Lt3 : Light_Ptr;
      Mat_Ptr       : Material_Ptr;
      Position      : Point_3D;
      Look_At       : Point_3D;
      Up            : Vector_3D;
   begin
      --  Build the world...
      Position := Construct_Point (0.0, 0.0, 30.0);
      Look_At  := Construct_Point (0.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 8.0, 25.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (1, "My Sampler");
      Smp_Ptr.Initialize;

      Mat_Ptr := Construct_Lambertian ("Dull White", WHITE_RGB_Spec, WHITE_RGB_Spec);
      Sphere  := Construct_Positioned_Sphere ("Sphere 1", ORIGIN_3D, 3.0);
      Add_Object (Sphere, Obj_List);
      Sphere.Set_Object_Material (Mat_Ptr);

      --  Set the lights
      --  Lt1 := Construct_Directional_Light ("My Direct Light 1", 0.33, WHITE_RGB_Spec, Construct_Vector (-1.0, 0.0, 0.35));
      Lt1 := Construct_Directional_Light ("My Direct Light 1", 0.33, WHITE_RGB_Spec, Construct_Vector (0.0, 0.0, -1.0), False);
      Add_Light (Lt1, Lt_List);
      --  Lt2 := Construct_Directional_Light ("My Direct Light 2", 0.33, WHITE_RGB_Spec, Construct_Vector (1.0, 0.0, 0.35));
      Lt2 := Construct_Directional_Light ("My Direct Light 2", 0.33, WHITE_RGB_Spec, Construct_Vector (0.0, 0.0, -1.0), False);
      Add_Light (Lt2, Lt_List);
      --  Lt3 := Construct_Directional_Light ("My Direct Light 3", 0.33, WHITE_RGB_Spec, Construct_Vector (0.0, 0.0, -1.0));
      Lt3 := Construct_Directional_Light ("My Direct Light 3", 0.33, WHITE_RGB_Spec, Construct_Vector (0.0, 0.0, -1.0), False);
      Add_Light (Lt3, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Name ("Dull_Sphere");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/dull_sphere.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;
   end Dull_Sphere;

   procedure Generic_Test_World is
      Cam_Ptr             : Camera_Ptr;
      Smp_Ptr             : Sampler_Ptr;
      Obj1, Obj2          : Object_Ptr;
      Trn1                : Matrix_3D;
      Obj_List            : Object_List;
      Lt_List             : Light_List;
      Dir_Lt1, Dir_Lt2    : Light_Ptr;
      Mat1_Ptr, Mat2_Ptr  : Material_Ptr;
      Position            : Point_3D;
      Look_At, Mid        : Point_3D;
      Up                  : Vector_3D;
      MaxZ, Col, nSPheres : Large_Integer := 0;
   begin
      --  Build the world...
      Position := Construct_Point (0.0, 50.0, 100.0);
      Look_At  := Construct_Point (0.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 5.0, 100.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (1, "My Sampler");
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      Mat1_Ptr := Construct_Phong ("Shiny Red", BLACK_RGB_Spec, RED_RGB_Spec, WHITE_RGB_Spec, Alpha => 100);
      Mat2_Ptr :=
        Construct_Reflective
          ("Mirror", BLACK_RGB_Spec, Construct_RGB_Spectrum (0.2, 0.2, 0.2), Construct_RGB_Spectrum (0.8, 0.8, 0.8),
           Construct_RGB_Spectrum (0.2, 0.2, 0.2), 0.2, 0.3, 0.5, 0.5, 200);

      Obj1 := Construct_Positioned_Cone ("Cone", Construct_Point (1.0, 0.0, 0.0), Construct_Point (-1.0, 0.0, 0.0), 1.0);
      Obj1.Set_Object_Material (Mat1_Ptr);
      Add_Object (Obj1, Obj_List);

      Obj2 := Construct_Unit_Cube ("Floor");
      Trn1 := Construct_Translation (0.0, -2.0, 0.0) * Construct_Scale (100.0, 1.0, 100.0);
      Obj2.Set_Object_Material (Mat2_Ptr);
      Obj2.Transform (Trn1);
      Add_Object (Obj2, Obj_List);

      --  Set the lights
      Dir_Lt1 := Construct_Point_Light ("My Point Light 1", 0.5, WHITE_RGB_Spec, Construct_Point (500.0, 500.0, 500.0), True);
      Add_Light (Dir_Lt1, Lt_List);

      Dir_Lt2 := Construct_Point_Light ("My Point Light 2", 0.5, WHITE_RGB_Spec, Construct_Point (-500.0, 500.0, 500.0), True);
      Add_Light (Dir_Lt2, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Name ("Test Scene");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/balls_grid_TEST.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;

   end Generic_Test_World;

   procedure CSG_Test is
      Cam_Ptr                                                    : Camera_Ptr;
      Smp_Ptr                                                    : Sampler_Ptr;
      Sp1, Sp2, Sp3, Sp4, Sp5, Sp6, Sp7, Sp8                     : Object_Ptr;
      Trn1                                                       : Matrix_3D;
      Obj_List, CSG_Obj_List                                     : Object_List;
      Lt_List                                                    : Light_List;
      Dir_Lt1, Dir_Lt2                                           : Light_Ptr;
      Mat1_Ptr, Mat2_Ptr, Mat3_Ptr, Mat4_Ptr, Mat5_Ptr, Mat6_Ptr : Material_Ptr;
      Position                                                   : Point_3D;
      Look_At                                                    : Point_3D;
      Up                                                         : Vector_3D;
      Rad, Sc1, Sc2                                              : Large_Float;
      Ka, Kd, Ks, Kr                                             : Small_Float;
      bPow                                                       : Integer;
   begin
      --  Build the world...
      Position := Construct_Point (9.0, 1.0, 5.0);
      Look_At  := Construct_Point (0.0, 2.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 2.0, 2.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (1, "My Sampler");
      Set_Max_Recursion_Level (100);
      Set_Minimum_Weight (0.000_1);
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      Ka   := 0.0;
      Kd   := 0.1;
      Ks   := 0.2;
      Kr   := 0.7;
      bPow := 100;

      Mat1_Ptr :=
        Construct_Reflective ("Shiny Blue", CYAN_RGB_Spec, CYAN_RGB_Spec, WHITE_RGB_Spec, CYAN_RGB_Spec, Ka, Kd, Ks, Kr, bPow);
      Mat2_Ptr :=
        Construct_Reflective
          ("Shiny Red", PURPLE_RGB_Spec, PURPLE_RGB_Spec, WHITE_RGB_Spec, PURPLE_RGB_Spec, Ka, Kd, Ks, Kr, bPow);
      Mat3_Ptr :=
        Construct_Reflective ("Shiny Green", GREEN_RGB_Spec, GREEN_RGB_Spec, WHITE_RGB_Spec, GREEN_RGB_Spec, Ka, Kd, Ks, Kr, bPow);
      Mat4_Ptr :=
        Construct_Reflective
          ("Shiny Yellow", YELLOW_RGB_Spec, YELLOW_RGB_Spec, WHITE_RGB_Spec, YELLOW_RGB_Spec, Ka, Kd, Ks, Kr, bPow);
      Mat5_Ptr :=
        Construct_Reflective
          ("Shiny White", WHITE_RGB_Spec, WHITE_RGB_Spec, WHITE_RGB_Spec, WHITE_RGB_Spec, 0.0, 0.1, 0.1, 0.8, 20);
      Mat6_Ptr :=
        Construct_Reflective
          ("Shiny White", WHITE_RGB_Spec, WHITE_RGB_Spec, WHITE_RGB_Spec, WHITE_RGB_Spec, 0.0, 0.1, 0.1, 0.8, 20);

      Rad := Distance (Construct_Point (1.0, 1.0, 1.0), Construct_Point (-1.0, 1.0, -1.0)) / 2.0;
      Sc1 := 15.0;
      Sc2 := 14.0;

      Sp1  := Construct_Unit_Cube ("Sp1");
      Trn1 := Construct_Translation (0.0, 0.0, 0.0) * Construct_Scale (Sc1, Sc1, Sc1);
      Sp1.Transform (Trn1);
      Sp2  := Construct_Unit_Sphere ("Sp2");
      Trn1 := Construct_Translation (0.0, 0.0, 0.0) * Construct_Scale (Sc2, Sc2, Sc2 - 0.0);
      Sp2.Transform (Trn1);
      Sp3 := CSG_Difference (Sp1, Sp2, CSG_Obj_List);
      Sp3.Set_Object_Material (Mat5_Ptr);

      Sp8  := Construct_Unit_Cube ("Floor");
      Trn1 := Construct_Translation (0.0, -1.0, 0.0) * Construct_Scale (Sc1, 0.1, Sc1);
      Sp8.Transform (Trn1);
      Sp8.Set_Object_Material (Mat6_Ptr);
      Add_Object (Sp8, Obj_List);

      Sp4 := Construct_Positioned_Sphere ("Sphere 1", Construct_Point (1.0, 3.0, 1.0), Rad);
      Sp4.Set_Object_Material (Mat1_Ptr);
      Add_Object (Sp4, Obj_List);

      Sp5 := Construct_Positioned_Sphere ("Sphere 2", Construct_Point (-1.0, 1.0, 1.0), Rad);
      Sp5.Set_Object_Material (Mat2_Ptr);
      Add_Object (Sp5, Obj_List);

      Sp6 := Construct_Positioned_Sphere ("Sphere 3", Construct_Point (-1.0, 3.0, -1.0), Rad);
      Sp6.Set_Object_Material (Mat3_Ptr);
      Add_Object (Sp6, Obj_List);

      Sp7 := Construct_Positioned_Sphere ("Sphere 4", Construct_Point (1.0, 1.0, -1.0), Rad);
      Sp7.Set_Object_Material (Mat4_Ptr);
      Add_Object (Sp7, Obj_List);

      --  Set the lights
      Dir_Lt1 := Construct_Point_Light ("My Point Light 1", 0.99, WHITE_RGB_Spec, Construct_Point (8.0, 1.0, 5.0), True);
      Add_Light (Dir_Lt1, Lt_List);
      Dir_Lt2 := Construct_Point_Light ("My Point Light 1", 0.5, WHITE_RGB_Spec, Construct_Point (1.0, 10.0, -1.0), True);
      --  Add_Light (Dir_Lt2, Lt_List);
      Use_Ambient_Light (True);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_CSG_Object_List (CSG_Obj_List);
      Set_CSG_Tree (Sp8);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.0, 0.0, 0.0));
      Set_Name ("Test Scene");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/CSG_TEST_Sphere.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;

   end CSG_Test;

   procedure CSG_Test2 is
      Cam_Ptr                                : Camera_Ptr;
      Smp_Ptr                                : Sampler_Ptr;
      Left_Obj, Right_Obj                    : Object_Ptr;
      Base_Obj                               : Object_Ptr;
      CSG_Obj                                : Object_Ptr;
      Trn1                                   : Matrix_3D;
      Obj_List, CSG_Obj_List                 : Object_List;
      Lt_List                                : Light_List;
      Dir_Lt1, Dir_Lt2                       : Light_Ptr;
      Mat1_Ptr, Mat2_Ptr, Mat3_Ptr, Mat4_Ptr : Material_Ptr;
      Position                               : Point_3D;
      Look_At                                : Point_3D;
      Up                                     : Vector_3D;
   begin
      --  Build the world...
      Position := Construct_Point (-30.0, 20.0, 50.0);
      Look_At  := Construct_Point (0.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 5.0, 100.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (9, "My Sampler");
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      Mat1_Ptr := Construct_Lambertian ("Dull Red", RED_RGB_Spec, RED_RGB_Spec);
      Mat2_Ptr := Construct_Lambertian ("Dull Yellow", YELLOW_RGB_Spec, YELLOW_RGB_Spec);
      Mat3_Ptr := Construct_Lambertian ("Dull White", WHITE_RGB_Spec, WHITE_RGB_Spec);
      Mat4_Ptr := Construct_Lambertian ("Dull Green", GREEN_RGB_Spec, GREEN_RGB_Spec);

      Base_Obj := Construct_Unit_Cube ("base");
      Trn1     := Construct_Translation (0.0, -3.0, 0.0) * Construct_Scale (6.0, 0.1, 6.0);
      Base_Obj.Transform (Trn1);
      Base_Obj.Set_Object_Material (Mat3_Ptr);
      Add_Object (Base_Obj, Obj_List);

      Right_Obj := Construct_Positioned_Sphere ("Right Sphere", Construct_Point (0.0, 0.0, 0.0), 1.2);
      Left_Obj  := Construct_Unit_Cube ("Left Cube");
      --  Right_Obj2 := Construct_Positioned_Cylinder ("Cylinder1", Construct_Point (0.0, -2.0, 0.0), Construct_Point (0.0, 2.0,
      --  0.0), 0.7);

      Right_Obj.Set_Object_Material (Mat4_Ptr);
      Left_Obj.Set_Object_Material (Mat1_Ptr);
      --  Right_Obj2.Set_Object_Material (Mat2_Ptr);

      CSG_Obj := CSG_Difference (Left_Obj, Right_Obj, CSG_Obj_List);
      --  CSG_Obj2 := CSG_Intersection (CSG_Obj, Right_Obj2, CSG_Obj_List);

      --  CSG_Obj2.Set_Object_Material (Mat2_Ptr); CSG_Obj2.Set_Object_Material (Mat2_Ptr);
--        Trn1 := Construct_Rotate_Z (1.0);
--        Trn1 := Construct_Translation (0.5, 1.0, 0.0);
--        CSG_Obj.Transform (Trn1);

      --  Set the lights
      Dir_Lt1 := Construct_Point_Light ("My Point Light 1", 0.5, WHITE_RGB_Spec, Construct_Point (-80.0, 100.0, 100.0), True);
      Add_Light (Dir_Lt1, Lt_List);
      Dir_Lt2 := Construct_Point_Light ("My Point Light 2", 0.5, WHITE_RGB_Spec, Construct_Point (0.0, 150.0, 00.0), True);
      Add_Light (Dir_Lt2, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_CSG_Object_List (CSG_Obj_List);
      Set_CSG_Tree (CSG_Obj);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Name ("CSG Test Scene");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/CSG_TEST2.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;

   end CSG_Test2;

   procedure Random_Spheres is
      Cam_Ptr        : Camera_Ptr;
      Smp_Ptr        : Sampler_Ptr;
      Obj1           : Object_Ptr;
      Trn1           : Matrix_3D;
      Obj_List       : Object_List;
      Lt_List        : Light_List;
      Dir_Lt1        : Light_Ptr;
      Mrp            : array (1 .. 10) of Material_Ptr;
      Position       : Point_3D;
      Look_At, Mid   : Point_3D;
      Up             : Vector_3D;
      nSPheres       : Large_Integer := 0;
      Sx, Sy, Sz, Sf : Large_Float   := 1.0;
      Rx, Ry, Rz, Tf : Large_Float   := 0.0;
      Col            : Integer;
      G              : Generator;

   begin
      --  Build the world...
      Position := Construct_Point (0.0, 200.0, -200.0);
      Look_At  := Construct_Point (0.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 10.0, 25.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (9, "My Sampler");
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      Mrp (1) := Construct_Lambertian ("Dull Red", RED_RGB_Spec, RED_RGB_Spec);
      Mrp (2) := Construct_Lambertian ("Dull Red", YELLOW_RGB_Spec, YELLOW_RGB_Spec);
      Mrp (3) := Construct_Lambertian ("Dull Red", BLUE_RGB_Spec, BLUE_RGB_Spec);
      Mrp (4) := Construct_Lambertian ("Dull Red", GREEN_RGB_Spec, GREEN_RGB_Spec);
      Mrp (5) := Construct_Lambertian ("Dull Red", CYAN_RGB_Spec, CYAN_RGB_Spec);
      Mrp (6) := Construct_Lambertian ("Dull Red", PURPLE_RGB_Spec, PURPLE_RGB_Spec);
      Mrp (7) := Construct_Lambertian ("Dull Red", WHITE_RGB_Spec, WHITE_RGB_Spec);
      Tf      := 20.0;
      Sf      := 1.0;

      Obj1 := Construct_Unit_Cube ("Floor");
      Trn1 := Construct_Translation (0.0, -Tf, 0.0) * Construct_Scale (100.0, 0.1, 100.0);
      Obj1.Transform (Trn1);
      Obj1.Set_Object_Material (Mrp (7));
      Add_Object (Obj1, Obj_List);

      for Z in 1 .. 100 loop

         nSPheres := nSPheres + 1;
         Obj1     := Construct_Unit_Sphere ("Sphere");
         Rx       := Tf * (2.0 * Large_Float (Random (G)) - 1.0);
         Ry       := Tf * (2.0 * Large_Float (Random (G)) - 1.0);
         Rz       := Tf * (2.0 * Large_Float (Random (G)) - 1.0);
         Sx       := Sf * (Large_Float (Random (G)));
         Sy       := Sf * (Large_Float (Random (G)));
         Sz       := Sf * (Large_Float (Random (G)));
         Trn1     := Construct_Translation (Rx, Ry, Rz) * Construct_Scale (Sx, Sy, Sz);
         Obj1.Transform (Trn1);
         Col := Integer (7.0 * Random (G) + 0.5);
         Obj1.Set_Object_Material (Mrp (Col));
         Add_Object (Obj1, Obj_List);

      end loop;
      Put ("Spheres added: ");
      Put (nSPheres);
      New_Line;

      --  Set the lights
      Dir_Lt1 := Construct_Point_Light ("My Point Light 1", 0.7, WHITE_RGB_Spec, Construct_Point (0.0, 100.0, 0.0), False);
      Add_Light (Dir_Lt1, Lt_List);
      Set_Ambient_Light (Construct_RGB_Spectrum (0.3, 0.3, 0.3));

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Name ("Test Scene");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/random_balls_3x3_regular.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;

   end Random_Spheres;

   procedure Cones_Comb is
      Cam_Ptr        : Camera_Ptr;
      Smp_Ptr        : Sampler_Ptr;
      Obj1           : Object_Ptr;
      Trn1           : Matrix_3D;
      Obj_List       : Object_List;
      Lt_List        : Light_List;
      Dir_Lt1        : Light_Ptr;
      Mrp            : array (1 .. 10) of Material_Ptr;
      Position       : Point_3D;
      Look_At, Mid   : Point_3D;
      Up             : Vector_3D;
      nCones         : Large_Integer := 1;
      Sx, Sy, Sz, Sf : Large_Float   := 1.0;
      Rx, Ry, Rz, Tf : Large_Float   := 0.0;
      G              : Generator;

   begin
      --  Build the world...
      Position := Construct_Point (0.0, 20.0, 100.0);
      Look_At  := Construct_Point (0.0, -10.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Basic_Camera (Position, Look_At, Up, 1_024, 256, 200.0, 100.0);
      -- Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (9, "USRS");
      Smp_Ptr := Construct_UnitSquare_Random_Sampler (25, 0.1, "USRS");
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      Mrp (1) := Construct_Lambertian ("Dull Red", RED_RGB_Spec, RED_RGB_Spec);
      Mrp (2) := Construct_Lambertian ("Dull Red", YELLOW_RGB_Spec, YELLOW_RGB_Spec);
      Mrp (3) := Construct_Lambertian ("Dull Red", BLUE_RGB_Spec, BLUE_RGB_Spec);
      Mrp (4) := Construct_Lambertian ("Dull Red", GREEN_RGB_Spec, GREEN_RGB_Spec);
      Mrp (5) := Construct_Lambertian ("Dull Red", CYAN_RGB_Spec, CYAN_RGB_Spec);
      Mrp (6) := Construct_Lambertian ("Dull Red", PURPLE_RGB_Spec, PURPLE_RGB_Spec);
      Mrp (7) := Construct_Lambertian ("Dull Red", WHITE_RGB_Spec, WHITE_RGB_Spec);

      Obj1 := Construct_Unit_Cube ("Floor");
      Trn1 := Construct_Translation (0.0, -10.0, 0.0) * Construct_Scale (1_000.0, 0.1, 1_000.0);
      Obj1.Transform (Trn1);
      Obj1.Set_Object_Material (Mrp (7));
      --  Add_Object (Obj1, Obj_List);

      Tf := 1.0;
      Sf := 0.75;
      Sx := 0.5;
      Sy := 30.0;
      Sz := 0.5;

      --  The 1st cone
      Obj1 := Construct_Unit_Cone ("Tooth");
      Trn1 := Construct_Translation (0.0, 0.0, 0.0) * Construct_Scale (Sx, Sy, Sz);
      Obj1.Transform (Trn1);
      Obj1.Set_Object_Material (Mrp (2));
      Add_Object (Obj1, Obj_List);

      for Z in 1 .. 100 loop

         Rx := Sf * Large_Float (Z);

         nCones := nCones + 1;
         Obj1   := Construct_Unit_Cone ("Tooth");
         Trn1   := Construct_Translation (Rx, 0.0, 0.0) * Construct_Scale (Sx, Sy, Sz);
         Obj1.Transform (Trn1);
         Obj1.Set_Object_Material (Mrp (2));
         Add_Object (Obj1, Obj_List);

         nCones := nCones + 1;
         Obj1   := Construct_Unit_Cone ("Tooth");
         Trn1   := Construct_Translation (-Rx, 0.0, 0.0) * Construct_Scale (Sx, Sy, Sz);
         Obj1.Transform (Trn1);
         Obj1.Set_Object_Material (Mrp (2));
         Add_Object (Obj1, Obj_List);

      end loop;
      Put ("Cones added: ");
      Put (nCones);
      New_Line;

      --  Set the lights
      Dir_Lt1 := Construct_Point_Light ("My Point Light 1", 0.7, WHITE_RGB_Spec, Construct_Point (0.0, 20.0, 100.0), False);
      Add_Light (Dir_Lt1, Lt_List);

      --  Dir_Lt2 := Construct_Directional_Light ("My Direct Light 2", 0.5, WHITE_RGB_Spec, Construct_Vector (0.1, -1.0, 1.0),
      --  False); Add_Light (Dir_Lt2, Lt_List);
      Set_Ambient_Light (Construct_RGB_Spectrum (0.3, 0.3, 0.3));

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Name ("Test Scene");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/cones_comb_6x6_regular.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);

      --  Render the scene
      Render_Scene;

   end Cones_Comb;

   procedure Coloured_Cones_Comb is
      Cam_Ptr        : Camera_Ptr;
      Smp_Ptr        : Sampler_Ptr;
      Obj1           : Object_Ptr;
      Trn1           : Matrix_3D;
      Obj_List       : Object_List;
      Lt_List        : Light_List;
      Dir_Lt1        : Light_Ptr;
      Mrp            : array (1 .. 10) of Material_Ptr;
      Position       : Point_3D;
      Look_At, Mid   : Point_3D;
      Up             : Vector_3D;
      nCones         : Large_Integer := 1;
      Sx, Sy, Sz, Sf : Large_Float   := 1.0;
      Rx, Ry, Rz, Tf : Large_Float   := 0.0;
      Col            : Integer;
      G              : Generator;

   begin
      --  Build the world...
      Position := Construct_Point (0.0, 20.0, 100.0);
      Look_At  := Construct_Point (0.0, -10.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Basic_Camera (Position, Look_At, Up, 1_024, 256, 200.0, 100.0);

      Mrp (1) := Construct_Lambertian ("Dull Red", RED_RGB_Spec, RED_RGB_Spec);
      Mrp (2) := Construct_Lambertian ("Dull Red", YELLOW_RGB_Spec, YELLOW_RGB_Spec);
      Mrp (3) := Construct_Lambertian ("Dull Red", BLUE_RGB_Spec, BLUE_RGB_Spec);
      Mrp (4) := Construct_Lambertian ("Dull Red", GREEN_RGB_Spec, GREEN_RGB_Spec);
      Mrp (5) := Construct_Lambertian ("Dull Red", CYAN_RGB_Spec, CYAN_RGB_Spec);
      Mrp (6) := Construct_Lambertian ("Dull Red", PURPLE_RGB_Spec, PURPLE_RGB_Spec);
      Mrp (7) := Construct_Lambertian ("Dull Red", WHITE_RGB_Spec, WHITE_RGB_Spec);

      Tf := 1.0;
      Sf := 0.95;
      Sx := 0.5;
      Sy := 30.0;
      Sz := 0.5;

      --  The 1st cone
      Obj1 := Construct_Unit_Cone ("Tooth");
      Trn1 := Construct_Translation (0.0, 0.0, 0.0) * Construct_Scale (Sx, Sy, Sz);
      Obj1.Transform (Trn1);
      Obj1.Set_Object_Material (Mrp (2));
      Add_Object (Obj1, Obj_List);

      for Z in -100 .. 100 loop

         Rx := Sf * Large_Float (Z);

         nCones := nCones + 1;
         Obj1   := Construct_Unit_Cone ("Tooth");
         Trn1   := Construct_Translation (Rx, 0.0, 0.0) * Construct_Scale (Sx, Sy, Sz);
         Obj1.Transform (Trn1);
         --  Randomize the colour
         --  Col := Integer(7.0 * Random (G) + 0.5);
         Col := 1 + Z mod 2;
         Obj1.Set_Object_Material (Mrp (Col));
         Add_Object (Obj1, Obj_List);

      end loop;
      Put ("Cones added: ");
      Put (nCones);
      New_Line;

      --  Set the light
      Dir_Lt1 := Construct_Point_Light ("My Point Light 1", 0.7, WHITE_RGB_Spec, Construct_Point (0.0, 20.0, 100.0), False);
      Add_Light (Dir_Lt1, Lt_List);
      Set_Ambient_Light (Construct_RGB_Spectrum (0.3, 0.3, 0.3));

      --  Build The_World
      Smp_Ptr := Construct_UnitSquare_Random_Sampler (1, 0.2, "My Sampler");
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Name ("Test Scene");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/redyellow_cones_comb_16x16_random_box.ppm");
      Set_Camera (Cam_Ptr);

      --  Render the scene
      Render_Scene;

   end Coloured_Cones_Comb;

end Build_Functions;
