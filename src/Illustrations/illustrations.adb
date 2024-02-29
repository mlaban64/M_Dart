with Core_Types;                use Core_Types;
with Linear_Math;               use Linear_Math;
with Scenes;                    use Scenes;
with Samplers;                  use Samplers;
with Samplers.UnitSquares;      use Samplers.UnitSquares;
with Objects;                   use Objects;
with Objects.Unit_Cubes;        use Objects.Unit_Cubes;
with Objects.Unit_Spheres;      use Objects.Unit_Spheres;
with Objects.Unit_Cylinders;    use Objects.Unit_Cylinders;
with Objects.Unit_Cones;        use Objects.Unit_Cones;
with Objects.CSG_Objects;       use Objects.CSG_Objects;
with Objects.Compounds;         use Objects.Compounds;
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
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
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

package body Illustrations is

   procedure Camera_Viewing_Parameters is
      Cam_Ptr                                            : Camera_Ptr;
      Smp_Ptr                                            : Sampler_Ptr;
      P_Cam, L_a, CSG_All                                : Object_Ptr;
      Gaze_Ptr, Up_Ptr, Scr_w, Scr_h                     : Object_Ptr;
      Scr_d, Grid_Ptr, C1                                : Object_Ptr;
      Trn1                                               : Matrix_3D;
      Obj_List                                           : Object_List;
      CSG_Obj_List                                       : Object_List;
      Lt_List                                            : Light_List;
      Dir_Lt1                                            : Light_Ptr;
      Red_Ptr, Yellow_Ptr, Green_Ptr, Blue_Ptr, Cyan_Ptr : Material_Ptr;
      Position                                           : Point_3D;
      Look_At                                            : Point_3D;
      Up                                                 : Vector_3D;

   begin
      --  Build the world...
      Position := Construct_Point (30.0, 20.0, 35.0);
      Look_At  := Construct_Point (0.0, 0.0, 3.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 12.0, 49.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (36, "My Sampler");

      Yellow_Ptr := Construct_Lambertian ("Bright Yellow", YELLOW_RGB_Spec, YELLOW_RGB_Spec);
      Red_Ptr    := Construct_Lambertian ("Bright Red", RED_RGB_Spec, RED_RGB_Spec);
      Green_Ptr  := Construct_Lambertian ("Bright Green", GREEN_RGB_Spec, GREEN_RGB_Spec);
      Blue_Ptr   := Construct_Lambertian ("Bright Blue", BLUE_RGB_Spec, BLUE_RGB_Spec);
      Cyan_Ptr   := Construct_Lambertian ("Bright Cyan", CYAN_RGB_Spec, CYAN_RGB_Spec);

      P_Cam := Construct_Positioned_Sphere ("P_cam", Construct_Point (0.0, 0.0, 10.0), 0.2);
      L_a   := Construct_Positioned_Sphere ("L_a", Construct_Point (0.0, 0.0, 0.0), 0.2);
      C1    := CSG_Union (P_Cam, L_a, CSG_Obj_List);
      C1.Set_Object_Material (Yellow_Ptr);

      Grid_Ptr := Construct_Grid (ORIGIN_3D, CSG_Obj_List, Blue_Ptr);
      CSG_All  := CSG_Union (Grid_Ptr, C1, CSG_Obj_List);

      Gaze_Ptr := Construct_Arrow (Construct_Point (0.0, 0.0, 10.0), Construct_Point (0.0, 0.0, 5.0), CSG_Obj_List, Red_Ptr);
      CSG_All  := CSG_Union (CSG_All, Gaze_Ptr, CSG_Obj_List);

      Up_Ptr  := Construct_Arrow (Construct_Point (0.0, 0.0, 10.0), Construct_Point (0.0, 5.0, 10.0), CSG_Obj_List, Red_Ptr);
      CSG_All := CSG_Union (CSG_All, Up_Ptr, CSG_Obj_List);

      Scr_w :=
        Construct_Double_Arrow (Construct_Point (-2.0, -2.5, 0.0), Construct_Point (2.0, -2.5, 0.0), CSG_Obj_List, Green_Ptr);
      CSG_All := CSG_Union (CSG_All, Scr_w, CSG_Obj_List);

      Scr_h :=
        Construct_Double_Arrow (Construct_Point (-2.5, -2.0, 0.0), Construct_Point (-2.5, 2.0, 0.0), CSG_Obj_List, Green_Ptr);
      CSG_All := CSG_Union (CSG_All, Scr_h, CSG_Obj_List);

      Scr_d :=
        Construct_Double_Arrow (Construct_Point (0.0, -0.5, 10.0), Construct_Point (0.0, -0.5, 0.0), CSG_Obj_List, Green_Ptr);
      CSG_All := CSG_Union (CSG_All, Scr_d, CSG_Obj_List);

      --  Lights
      Dir_Lt1 := Construct_Point_Light ("Point Light", 0.75, WHITE_RGB_Spec, Position, False);
      Add_Light (Dir_Lt1, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_CSG_Object_List (CSG_Obj_List);
      Set_CSG_Tree (CSG_All);
      Set_Light_List (Lt_List);
      --  Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Background_Color (WHITE_RGB_Spec);
      Set_Name ("Backward Ray Tracing");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/camera_viewing_parameters.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;

   end Camera_Viewing_Parameters;

   procedure Gallery_II_Chess_Board is
      Cam_Ptr                               : Camera_Ptr;
      Smp_Ptr                               : Sampler_Ptr;
      Tile, Boarder                         : Object_Ptr;
      Trn1                                  : Matrix_3D;
      Obj_List                              : Object_List;
      CSG_Obj_List                          : Object_List;
      Lt_List                               : Light_List;
      Lt1, Lt2, Lt3                         : Light_Ptr;
      Black_Tile, White_Tile                : Material_Ptr;
      Boarder_Mat, Black_Piece, White_Piece : Material_Ptr;
      WPawn1, WPawn2, WPawn3, WPawn4        : Object_Ptr;
      WPawn5, WPawn6, WPawn7, WPawn8        : Object_Ptr;
      BPawn1, BPawn2, BPawn3, BPawn4        : Object_Ptr;
      BPawn5, BPawn6, BPawn7, BPawn8        : Object_Ptr;
      WKnight1, WRook1, WBishop1            : Object_Ptr;
      WKnight2, WRook2, WBishop2            : Object_Ptr;
      BKnight1, BRook1, BBishop1            : Object_Ptr;
      BKnight2, BRook2, BBishop2            : Object_Ptr;
      WQueen, BQueen, WKing, BKing          : Object_Ptr;
      ChessBoard                            : Object_Ptr;
      Position                              : Point_3D;
      Look_At                               : Point_3D;
      Up                                    : Vector_3D;
      Odd, Odd2                             : Boolean := False;
      gX, gZ                                : Large_Float;

   begin
      --  Build the world...
      Position := Construct_Point (70.0, 25.0, 20.0);
      --  Position := Construct_Point(6.8, 2.25, 6.7);
      Look_At := Construct_Point (0.0, 0.0, 0.0);
      Up      := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 20.0, 100.0);
      Smp_Ptr := Construct_UnitSquare_Regular_Sampler (1, "My Sampler");

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

      White_Piece :=
        Construct_Phong
          ("White Piece", Construct_RGB_Spectrum (0.2, 0.2, 0.2), Construct_RGB_Spectrum (0.8, 0.8, 0.8),
           Construct_RGB_Spectrum (0.99, 0.99, 0.99), 1.0, 1.0, 1.0, 100);

      Black_Piece :=
        Construct_Phong
          ("Black Piece", Construct_RGB_Spectrum (0.1, 0.1, 0.1), Construct_RGB_Spectrum (0.2, 0.2, 0.2),
           Construct_RGB_Spectrum (0.9, 0.9, 0.9), 1.0, 1.0, 1.0, 100);

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

      --  The Chess Pieces
      --  First the pawns...
      --  White Pawns
      WPawn1 := Construct_Pawn (Construct_Point (-7.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn2 := Construct_Pawn (Construct_Point (-5.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn3 := Construct_Pawn (Construct_Point (-3.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn4 := Construct_Pawn (Construct_Point (-1.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn5 := Construct_Pawn (Construct_Point (1.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn6 := Construct_Pawn (Construct_Point (3.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn7 := Construct_Pawn (Construct_Point (5.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn8 := Construct_Pawn (Construct_Point (7.0, 0.0, 5.0), CSG_Obj_List, White_Piece);

      --  Black Pawns
      BPawn1 := Construct_Pawn (Construct_Point (-7.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn2 := Construct_Pawn (Construct_Point (-5.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn3 := Construct_Pawn (Construct_Point (-3.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn4 := Construct_Pawn (Construct_Point (-1.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn5 := Construct_Pawn (Construct_Point (1.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn6 := Construct_Pawn (Construct_Point (3.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn7 := Construct_Pawn (Construct_Point (5.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn8 := Construct_Pawn (Construct_Point (7.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);

      --  White Rooks
      WRook1 := Construct_Rook (Construct_Point (-7.0, 0.0, 7.0), CSG_Obj_List, White_Piece);
      WRook2 := Construct_Rook (Construct_Point (7.0, 0.0, 7.0), CSG_Obj_List, White_Piece);

      --  Black Rooks
      BRook1 := Construct_Rook (Construct_Point (-7.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);
      BRook2 := Construct_Rook (Construct_Point (7.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  White Bishops
      WBishop1 := Construct_Bishop (Construct_Point (0.0, 0.0, 0.0), CSG_Obj_List, White_Piece);
      WBishop1.Transform (Construct_Translation (-3.0, 0.0, 7.0) * Construct_Rotate_Y (Ray_PI));
      WBishop2 := Construct_Bishop (Construct_Point (0.0, 0.0, 0.0), CSG_Obj_List, White_Piece);
      WBishop2.Transform (Construct_Translation (3.0, 0.0, 7.0) * Construct_Rotate_Y (Ray_PI));

      --  Black Bishops
      BBishop1 := Construct_Bishop (Construct_Point (-3.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);
      BBishop2 := Construct_Bishop (Construct_Point (3.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  White Knights
      WKnight1 := Construct_Knight (Construct_Point (0.0, 0.0, 0.0), CSG_Obj_List, White_Piece);
      WKnight1.Transform (Construct_Translation (-5.0, 0.0, 7.0) * Construct_Rotate_Y (Ray_PI));
      WKnight2 := Construct_Knight (Construct_Point (0.0, 0.0, 0.0), CSG_Obj_List, White_Piece);
      WKnight2.Transform (Construct_Translation (5.0, 0.0, 7.0) * Construct_Rotate_Y (Ray_PI));

      --  Black Knights
      BKnight1 := Construct_Knight (Construct_Point (-5.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);
      BKnight2 := Construct_Knight (Construct_Point (5.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  Queens
      WQueen := Construct_Queen (Construct_Point (-1.0, 0.0, 7.0), CSG_Obj_List, White_Piece);
      BQueen := Construct_Queen (Construct_Point (1.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  Kings
      WKing := Construct_King (Construct_Point (1.0, 0.0, 7.0), CSG_Obj_List, White_Piece);
      BKing := Construct_King (Construct_Point (-1.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  Add it all up
      ChessBoard := CSG_Union (WPawn1, WPawn2, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn3, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn4, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn5, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn6, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn7, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn8, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, BPawn1, CSG_Obj_List);
      Set_CSG_Object_List (CSG_Obj_List);
      Set_CSG_Tree (ChessBoard);

      --  Lights
      Lt1 := Construct_Point_Light ("Point Light", 0.35, WHITE_RGB_Spec, Construct_Point (0.0, 10.0, 1.0), True);
      Add_Light (Lt1, Lt_List);
      Lt2 := Construct_Point_Light ("Point Light", 0.35, WHITE_RGB_Spec, Construct_Point (20.0, 5.0, 20.0), True);
      Add_Light (Lt2, Lt_List);
      Lt3 := Construct_Point_Light ("Point Light", 0.35, WHITE_RGB_Spec, Construct_Point (20.0, 15.0, 8.0), True);
      Add_Light (Lt3, Lt_List);
      Set_Ambient_Light (Construct_RGB_Spectrum (0.2, 0.2, 0.2));

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.4, 0.4, 0.7));

      Set_Name ("Chess Board");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/gallery_ii_chess_board.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;

   end Gallery_II_Chess_Board;

   procedure Gallery_III_Chess_Board_For_STS is
      Cam_Ptr                               : Camera_Ptr;
      Smp_Ptr                               : Sampler_Ptr;
      Tile, Boarder                         : Object_Ptr;
      Trn1                                  : Matrix_3D;
      Obj_List                              : Object_List;
      CSG_Obj_List                          : Object_List;
      Lt_List                               : Light_List;
      Lt1, Lt2, Lt3                         : Light_Ptr;
      Black_Tile, White_Tile                : Material_Ptr;
      Boarder_Mat, Black_Piece, White_Piece : Material_Ptr;
      WPawn1, WPawn2, WPawn3, WPawn4        : Object_Ptr;
      WPawn5, WPawn6, WPawn7, WPawn8        : Object_Ptr;
      BPawn1, BPawn2, BPawn3, BPawn4        : Object_Ptr;
      BPawn5, BPawn6, BPawn7, BPawn8        : Object_Ptr;
      WKnight1, WRook1, WBishop1            : Object_Ptr;
      WKnight2, WRook2, WBishop2            : Object_Ptr;
      BKnight1, BRook1, BBishop1            : Object_Ptr;
      BKnight2, BRook2, BBishop2            : Object_Ptr;
      WQueen, BQueen, WKing, BKing          : Object_Ptr;
      ChessBoard                            : Object_Ptr;
      Position                              : Point_3D;
      Look_At                               : Point_3D;
      Up                                    : Vector_3D;
      Odd, Odd2                             : Boolean := False;
      gX, gZ                                : Large_Float;

   begin
      --  Build the world...
      Position := Construct_Point (70.0, 25.0, -20.0);
      --  Position := Construct_Point(6.8, 2.25, 6.7);
      Look_At := Construct_Point (0.0, 0.0, 0.0);
      Up      := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr := Construct_Pinhole_Camera (Position, Look_At, Up, 3_000, 750, 20.0, 100.0);
      Smp_Ptr := Construct_UnitSquare_Regular_Sampler (4, "My Sampler");

      White_Tile :=
        Construct_Reflective
          ("White Tile", Construct_RGB_Spectrum (0.2, 0.2, 0.2), Construct_RGB_Spectrum (0.9, 0.9, 0.9),
           Construct_RGB_Spectrum (1.0, 1.0, 1.0), Construct_RGB_Spectrum (0.5, 0.5, 0.5), 0.1, 0.2, 0.3, 0.8, 100);

      Black_Tile :=
        Construct_Reflective
          ("Black Tile", Construct_RGB_Spectrum (0.1, 0.1, 0.1), Construct_RGB_Spectrum (0.3, 0.3, 0.3),
           Construct_RGB_Spectrum (1.0, 1.0, 1.0), Construct_RGB_Spectrum (0.3, 0.3, 0.3), 0.2, 0.3, 0.3, 0.8, 100);

      Boarder_Mat :=
        Construct_Phong
          ("Boarder", Construct_RGB_Spectrum (0.7, 0.7, 0.7), Construct_RGB_Spectrum (0.7, 0.7, 0.7),
           Construct_RGB_Spectrum (1.0, 1.0, 1.0), 0.2, 0.3, 0.5, 10);

      White_Piece :=
        Construct_Phong
          ("White Piece", Construct_RGB_Spectrum (1.0, 1.0, 1.0), Construct_RGB_Spectrum (1.0, 1.0, 1.0),
           Construct_RGB_Spectrum (1.0, 1.0, 1.0), 0.2, 0.5, 0.5, 100);

      Black_Piece :=
        Construct_Phong
          ("Black Piece", Construct_RGB_Spectrum (0.2, 0.2, 0.2), Construct_RGB_Spectrum (0.3, 0.3, 0.3),
           Construct_RGB_Spectrum (1.0, 1.0, 1.0), 0.2, 0.4, 0.4, 100);

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

      --  The Chess Pieces
      --  First the pawns...
      --  White Pawns
      WPawn1 := Construct_Pawn (Construct_Point (-7.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn2 := Construct_Pawn (Construct_Point (-5.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn3 := Construct_Pawn (Construct_Point (-3.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn4 := Construct_Pawn (Construct_Point (-1.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn5 := Construct_Pawn (Construct_Point (1.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn6 := Construct_Pawn (Construct_Point (3.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn7 := Construct_Pawn (Construct_Point (5.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn8 := Construct_Pawn (Construct_Point (7.0, 0.0, 5.0), CSG_Obj_List, White_Piece);

      --  Black Pawns
      BPawn1 := Construct_Pawn (Construct_Point (-7.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn2 := Construct_Pawn (Construct_Point (-5.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn3 := Construct_Pawn (Construct_Point (-3.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn4 := Construct_Pawn (Construct_Point (-1.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn5 := Construct_Pawn (Construct_Point (1.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn6 := Construct_Pawn (Construct_Point (3.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn7 := Construct_Pawn (Construct_Point (5.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn8 := Construct_Pawn (Construct_Point (7.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);

      --  White Rooks
      WRook1 := Construct_Rook (Construct_Point (-7.0, 0.0, 7.0), CSG_Obj_List, White_Piece);
      WRook2 := Construct_Rook (Construct_Point (7.0, 0.0, 7.0), CSG_Obj_List, White_Piece);

      --  Black Rooks
      BRook1 := Construct_Rook (Construct_Point (-7.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);
      BRook2 := Construct_Rook (Construct_Point (7.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  White Bishops
      WBishop1 := Construct_Bishop (Construct_Point (0.0, 0.0, 0.0), CSG_Obj_List, White_Piece);
      WBishop1.Transform (Construct_Translation (-3.0, 0.0, 7.0) * Construct_Rotate_Y (Ray_PI));
      WBishop2 := Construct_Bishop (Construct_Point (0.0, 0.0, 0.0), CSG_Obj_List, White_Piece);
      WBishop2.Transform (Construct_Translation (3.0, 0.0, 7.0) * Construct_Rotate_Y (Ray_PI));

      --  Black Bishops
      BBishop1 := Construct_Bishop (Construct_Point (-3.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);
      BBishop2 := Construct_Bishop (Construct_Point (3.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  White Knights
      WKnight1 := Construct_Knight (Construct_Point (0.0, 0.0, 0.0), CSG_Obj_List, White_Piece);
      WKnight1.Transform (Construct_Translation (-5.0, 0.0, 7.0) * Construct_Rotate_Y (Ray_PI));
      WKnight2 := Construct_Knight (Construct_Point (0.0, 0.0, 0.0), CSG_Obj_List, White_Piece);
      WKnight2.Transform (Construct_Translation (5.0, 0.0, 7.0) * Construct_Rotate_Y (Ray_PI));

      --  Black Knights
      BKnight1 := Construct_Knight (Construct_Point (-5.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);
      BKnight2 := Construct_Knight (Construct_Point (5.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  Queens
      WQueen := Construct_Queen (Construct_Point (-1.0, 0.0, 7.0), CSG_Obj_List, White_Piece);
      BQueen := Construct_Queen (Construct_Point (1.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  Kings
      WKing := Construct_King (Construct_Point (1.0, 0.0, 7.0), CSG_Obj_List, White_Piece);
      BKing := Construct_King (Construct_Point (-1.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  Add it all up
      ChessBoard := CSG_Union (WPawn1, WPawn2, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn3, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn4, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn5, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn6, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn7, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn8, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, BPawn1, CSG_Obj_List);
      Set_CSG_Object_List (CSG_Obj_List);
      Set_CSG_Tree (ChessBoard);

      --  Lights
      Lt1 := Construct_Point_Light ("Point Light", 0.5, WHITE_RGB_Spec, Construct_Point (0.0, 10.0, 1.0), True);
      Add_Light (Lt1, Lt_List);
      Lt2 := Construct_Point_Light ("Point Light", 0.5, WHITE_RGB_Spec, Construct_Point (20.0, 5.0, 20.0), True);
      Add_Light (Lt2, Lt_List);
      Lt3 := Construct_Point_Light ("Point Light", 0.66, WHITE_RGB_Spec, Construct_Point (70.0, 25.0, -20.0), True);
      Add_Light (Lt3, Lt_List);
      Set_Ambient_Light (Construct_RGB_Spectrum (0.2, 0.2, 0.2));

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.4, 0.4, 0.4));

      Set_Name ("Chess Board");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/gallery_iii_chess_board.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;

   end Gallery_III_Chess_Board_For_STS;

   procedure Gallery_III_Chess_Board is
      Cam_Ptr                               : Camera_Ptr;
      Smp_Ptr                               : Sampler_Ptr;
      Tile, Boarder                         : Object_Ptr;
      Trn1                                  : Matrix_3D;
      Obj_List                              : Object_List;
      CSG_Obj_List                          : Object_List;
      Lt_List                               : Light_List;
      Lt1, Lt2, Lt3                         : Light_Ptr;
      Black_Tile, White_Tile                : Material_Ptr;
      Boarder_Mat, Black_Piece, White_Piece : Material_Ptr;
      WPawn1, WPawn2, WPawn3, WPawn4        : Object_Ptr;
      WPawn5, WPawn6, WPawn7, WPawn8        : Object_Ptr;
      BPawn1, BPawn2, BPawn3, BPawn4        : Object_Ptr;
      BPawn5, BPawn6, BPawn7, BPawn8        : Object_Ptr;
      WKnight1, WRook1, WBishop1            : Object_Ptr;
      WKnight2, WRook2, WBishop2            : Object_Ptr;
      BKnight1, BRook1, BBishop1            : Object_Ptr;
      BKnight2, BRook2, BBishop2            : Object_Ptr;
      WQueen, BQueen, WKing, BKing          : Object_Ptr;
      ChessBoard                            : Object_Ptr;
      Position                              : Point_3D;
      Look_At                               : Point_3D;
      Up                                    : Vector_3D;
      Odd, Odd2                             : Boolean := False;
      gX, gZ                                : Large_Float;

   begin
      --  Build the world...
      Position := Construct_Point (70.0, 25.0, -20.0);
      --  Position := Construct_Point(6.8, 2.25, 6.7);
      Look_At := Construct_Point (0.0, 0.0, 0.0);
      Up      := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr := Construct_Pinhole_Camera (Position, Look_At, Up, 1_500, 1_500, 20.0, 100.0);
      -- Smp_Ptr := Construct_UnitSquare_Regular_Sampler (1, "My Sampler");
      Smp_Ptr := Construct_UnitSquare_Random_Sampler (9, 0.2, "My Sampler");

      White_Tile :=
        Construct_Reflective
          ("White Tile", Construct_RGB_Spectrum (0.3, 0.3, 0.3), Construct_RGB_Spectrum (0.9, 0.8, 0.7),
           Construct_RGB_Spectrum (1.0, 1.0, 1.0), Construct_RGB_Spectrum (0.5, 0.5, 0.5), 0.25, 0.35, 0.35, 0.8, 100);

      Black_Tile :=
        Construct_Reflective
          ("Black Tile", Construct_RGB_Spectrum (0.4, 0.3, 0.2), Construct_RGB_Spectrum (0.4, 0.3, 0.2),
           Construct_RGB_Spectrum (1.0, 1.0, 1.0), Construct_RGB_Spectrum (0.5, 0.4, 0.3), 0.25, 0.35, 0.35, 0.8, 100);

      Boarder_Mat :=
        Construct_Phong
          ("Boarder", Construct_RGB_Spectrum (0.7, 0.7, 0.7), Construct_RGB_Spectrum (0.7, 0.7, 0.7),
           Construct_RGB_Spectrum (1.0, 1.0, 1.0), 0.2, 0.3, 0.5, 10);

      White_Piece :=
        Construct_Phong
          ("White Piece", Construct_RGB_Spectrum (1.0, 1.0, 1.0), Construct_RGB_Spectrum (1.0, 1.0, 1.0),
           Construct_RGB_Spectrum (1.0, 1.0, 1.0), 0.2, 0.5, 0.8, 100);

      Black_Piece :=
        Construct_Phong
          ("Black Piece", Construct_RGB_Spectrum (0.4, 0.3, 0.2), Construct_RGB_Spectrum (0.4, 0.3, 0.2),
           Construct_RGB_Spectrum (1.0, 1.0, 1.0), 0.2, 0.4, 0.8, 100);

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

      --  The Chess Pieces
      --  First the pawns...
      --  White Pawns
      WPawn1 := Construct_Pawn (Construct_Point (-7.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn2 := Construct_Pawn (Construct_Point (-5.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn3 := Construct_Pawn (Construct_Point (-3.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn4 := Construct_Pawn (Construct_Point (-1.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn5 := Construct_Pawn (Construct_Point (1.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn6 := Construct_Pawn (Construct_Point (3.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn7 := Construct_Pawn (Construct_Point (5.0, 0.0, 5.0), CSG_Obj_List, White_Piece);
      WPawn8 := Construct_Pawn (Construct_Point (7.0, 0.0, 5.0), CSG_Obj_List, White_Piece);

      --  Black Pawns
      BPawn1 := Construct_Pawn (Construct_Point (-7.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn2 := Construct_Pawn (Construct_Point (-5.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn3 := Construct_Pawn (Construct_Point (-3.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn4 := Construct_Pawn (Construct_Point (-1.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn5 := Construct_Pawn (Construct_Point (1.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn6 := Construct_Pawn (Construct_Point (3.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn7 := Construct_Pawn (Construct_Point (5.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);
      BPawn8 := Construct_Pawn (Construct_Point (7.0, 0.0, -5.0), CSG_Obj_List, Black_Piece);

      --  White Rooks
      WRook1 := Construct_Rook (Construct_Point (-7.0, 0.0, 7.0), CSG_Obj_List, White_Piece);
      WRook2 := Construct_Rook (Construct_Point (7.0, 0.0, 7.0), CSG_Obj_List, White_Piece);

      --  Black Rooks
      BRook1 := Construct_Rook (Construct_Point (-7.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);
      BRook2 := Construct_Rook (Construct_Point (7.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  White Bishops
      WBishop1 := Construct_Bishop (Construct_Point (0.0, 0.0, 0.0), CSG_Obj_List, White_Piece);
      WBishop1.Transform (Construct_Translation (-3.0, 0.0, 7.0) * Construct_Rotate_Y (Ray_PI));
      WBishop2 := Construct_Bishop (Construct_Point (0.0, 0.0, 0.0), CSG_Obj_List, White_Piece);
      WBishop2.Transform (Construct_Translation (3.0, 0.0, 7.0) * Construct_Rotate_Y (Ray_PI));

      --  Black Bishops
      BBishop1 := Construct_Bishop (Construct_Point (-3.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);
      BBishop2 := Construct_Bishop (Construct_Point (3.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  White Knights
      WKnight1 := Construct_Knight (Construct_Point (0.0, 0.0, 0.0), CSG_Obj_List, White_Piece);
      WKnight1.Transform (Construct_Translation (-5.0, 0.0, 7.0) * Construct_Rotate_Y (Ray_PI));
      WKnight2 := Construct_Knight (Construct_Point (0.0, 0.0, 0.0), CSG_Obj_List, White_Piece);
      WKnight2.Transform (Construct_Translation (5.0, 0.0, 7.0) * Construct_Rotate_Y (Ray_PI));

      --  Black Knights
      BKnight1 := Construct_Knight (Construct_Point (-5.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);
      BKnight2 := Construct_Knight (Construct_Point (5.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  Queens
      WQueen := Construct_Queen (Construct_Point (-1.0, 0.0, 7.0), CSG_Obj_List, White_Piece);
      BQueen := Construct_Queen (Construct_Point (1.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  Kings
      WKing := Construct_King (Construct_Point (1.0, 0.0, 7.0), CSG_Obj_List, White_Piece);
      BKing := Construct_King (Construct_Point (-1.0, 0.0, -7.0), CSG_Obj_List, Black_Piece);

      --  Add it all up
      ChessBoard := CSG_Union (WPawn1, WPawn2, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn3, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn4, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn5, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn6, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn7, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, WPawn8, CSG_Obj_List);
      ChessBoard := CSG_Union (ChessBoard, BPawn1, CSG_Obj_List);
      Set_CSG_Object_List (CSG_Obj_List);
      Set_CSG_Tree (ChessBoard);

      --  Lights
      Lt1 := Construct_Point_Light ("Point Light", 0.9, WHITE_RGB_Spec, Construct_Point (0.0, 100.0, 1.0), True);
      Add_Light (Lt1, Lt_List);
      Lt2 := Construct_Point_Light ("Point Light", 0.5, WHITE_RGB_Spec, Construct_Point (-10.0, 50.0, 0.0), True);
      Add_Light (Lt2, Lt_List);
      Lt3 := Construct_Point_Light ("Point Light", 0.9, WHITE_RGB_Spec, Construct_Point (70.0, 25.0, -20.0), True);
      Add_Light (Lt3, Lt_List);
      Set_Ambient_Light (Construct_RGB_Spectrum (0.25, 0.25, 0.25));

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.3, 0.4, 0.6));

      Set_Name ("Chess Board");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/gallery_iii_chess_board.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;

   end Gallery_III_Chess_Board;

   procedure Illustration_Backward_Ray_Tracing is
      Cam_Ptr                                            : Camera_Ptr;
      Smp_Ptr                                            : Sampler_Ptr;
      Arrow_Ptr, Star_Ptr, Face_Ptr, CSG_All             : Object_Ptr;
      Arrow_Ptr1, Arrow_Ptr2, Arrow_Ptr3                 : Object_Ptr;
      Grid_Ptr                                           : Object_Ptr;
      Cylinder_Ptr                                       : Object_Ptr;
      Trn1                                               : Matrix_3D;
      Obj_List                                           : Object_List;
      CSG_Obj_List                                       : Object_List;
      Lt_List                                            : Light_List;
      Dir_Lt1                                            : Light_Ptr;
      Red_Ptr, Yellow_Ptr, Green_Ptr, Blue_Ptr, Cyan_Ptr : Material_Ptr;
      Position                                           : Point_3D;
      Look_At                                            : Point_3D;
      Up                                                 : Vector_3D;

   begin
      --  Build the world...
      Position := Construct_Point (30.0, 20.0, 100.0);
      Look_At  := Construct_Point (-1.0, 1.5, -5.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 13.0, 100.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (100, "My Sampler");

      Yellow_Ptr := Construct_Lambertian ("Bright Yellow", YELLOW_RGB_Spec, YELLOW_RGB_Spec);
      Red_Ptr    := Construct_Lambertian ("Bright Red", RED_RGB_Spec, RED_RGB_Spec);
      Green_Ptr  := Construct_Lambertian ("Bright Green", GREEN_RGB_Spec, GREEN_RGB_Spec);
      Blue_Ptr   := Construct_Lambertian ("Bright Blue", BLUE_RGB_Spec, BLUE_RGB_Spec);
      Cyan_Ptr   := Construct_Lambertian ("Bright Cyan", CYAN_RGB_Spec, CYAN_RGB_Spec);

      Star_Ptr   := Construct_Star (Construct_Point (5.0, 5.0, -5.0), CSG_Obj_List, Yellow_Ptr);
      Arrow_Ptr1 := Construct_Arrow (Construct_Point (0.5, 2.0, -3.5), Construct_Point (5.0, 5.0, -5.0), CSG_Obj_List, Red_Ptr);
      Arrow_Ptr2 := Construct_Arrow (Construct_Point (0.0, 0.0, 9.0), Construct_Point (0.5, 2.0, -3.5), CSG_Obj_List, Red_Ptr);
      Arrow_Ptr3 := Construct_Arrow (Construct_Point (0.0, 0.0, 9.0), Construct_Point (2.0, 0.5, -3.0), CSG_Obj_List, Red_Ptr);

      Arrow_Ptr := CSG_Union (Arrow_Ptr1, Arrow_Ptr2, CSG_Obj_List);
      Arrow_Ptr := CSG_Union (Arrow_Ptr, Arrow_Ptr3, CSG_Obj_List);
      Grid_Ptr  := Construct_Grid (ORIGIN_3D, CSG_Obj_List, Blue_Ptr);
      Face_Ptr  := Construct_Observer (Construct_Point (0.0, 0.0, 10.0), CSG_Obj_List, Blue_Ptr);
      --  The non-CSG objects
      Cylinder_Ptr :=
        Construct_Positioned_Cylinder ("Cylinder", Construct_Point (-1.0, 0.0, -5.0), Construct_Point (-1.0, 3.0, -5.0), 2.0);
      Cylinder_Ptr.Set_Object_Material (Green_Ptr);
      Add_Object (Cylinder_Ptr, Obj_List);
      --  Add it all together
      CSG_All := CSG_Union (Grid_Ptr, Arrow_Ptr, CSG_Obj_List);
      CSG_All := CSG_Union (CSG_All, Face_Ptr, CSG_Obj_List);
      CSG_All := CSG_Union (CSG_All, Star_Ptr, CSG_Obj_List);

      --  Lights
      Dir_Lt1 := Construct_Point_Light ("Point Light", 0.75, WHITE_RGB_Spec, Position, False);
      Add_Light (Dir_Lt1, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_CSG_Object_List (CSG_Obj_List);
      Set_CSG_Tree (CSG_All);
      Set_Light_List (Lt_List);
      --  Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Background_Color (WHITE_RGB_Spec);
      Set_Name ("Backward Ray Tracing");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/backward_ray_tracing.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;

   end Illustration_Backward_Ray_Tracing;

   procedure Illustration_Forward_Ray_Tracing is
      Cam_Ptr                                            : Camera_Ptr;
      Smp_Ptr                                            : Sampler_Ptr;
      Arrow_Ptr, Star_Ptr, Face_Ptr, CSG_All             : Object_Ptr;
      Arrow_Ptr1, Arrow_Ptr2, Arrow_Ptr3                 : Object_Ptr;
      Arrow_Ptr4, Arrow_Ptr5                             : Object_Ptr;
      Grid_Ptr                                           : Object_Ptr;
      Cylinder_Ptr                                       : Object_Ptr;
      Trn1                                               : Matrix_3D;
      Obj_List                                           : Object_List;
      CSG_Obj_List                                       : Object_List;
      Lt_List                                            : Light_List;
      Dir_Lt1                                            : Light_Ptr;
      Red_Ptr, Yellow_Ptr, Green_Ptr, Blue_Ptr, Cyan_Ptr : Material_Ptr;
      Position                                           : Point_3D;
      Look_At                                            : Point_3D;
      Up                                                 : Vector_3D;

   begin
      --  Build the world...
      Position := Construct_Point (30.0, 20.0, 100.0);
      Look_At  := Construct_Point (-1.0, 1.5, -5.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 13.0, 100.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (100, "My Sampler");

      Yellow_Ptr := Construct_Lambertian ("Bright Yellow", YELLOW_RGB_Spec, YELLOW_RGB_Spec);
      Red_Ptr    := Construct_Lambertian ("Bright Red", RED_RGB_Spec, RED_RGB_Spec);
      Green_Ptr  := Construct_Lambertian ("Bright Green", GREEN_RGB_Spec, GREEN_RGB_Spec);
      Blue_Ptr   := Construct_Lambertian ("Bright Blue", BLUE_RGB_Spec, BLUE_RGB_Spec);
      Cyan_Ptr   := Construct_Lambertian ("Bright Cyan", CYAN_RGB_Spec, CYAN_RGB_Spec);

      Star_Ptr   := Construct_Star (Construct_Point (5.0, 5.0, -5.0), CSG_Obj_List, Yellow_Ptr);
      Arrow_Ptr1 := Construct_Arrow (Construct_Point (5.0, 5.0, -5.0), Construct_Point (0.5, 2.0, -3.5), CSG_Obj_List, Red_Ptr);
      Arrow_Ptr2 := Construct_Arrow (Construct_Point (0.5, 2.0, -3.5), Construct_Point (0.0, 0.0, 9.0), CSG_Obj_List, Red_Ptr);
      Arrow_Ptr3 := Construct_Arrow (Construct_Point (5.0, 5.0, -5.0), Construct_Point (0.0, 0.0, 9.0), CSG_Obj_List, Cyan_Ptr);
      Arrow_Ptr4 := Construct_Arrow (Construct_Point (5.0, 5.0, -5.0), Construct_Point (-1.0, 3.0, -5.0), CSG_Obj_List, Cyan_Ptr);
      Arrow_Ptr5 := Construct_Arrow (Construct_Point (-1.0, 3.0, -5.0), Construct_Point (-4.0, 5.0, -5.0), CSG_Obj_List, Cyan_Ptr);
      Arrow_Ptr  := CSG_Union (Arrow_Ptr1, Arrow_Ptr2, CSG_Obj_List);
      Arrow_Ptr  := CSG_Union (Arrow_Ptr, Arrow_Ptr3, CSG_Obj_List);
      Arrow_Ptr  := CSG_Union (Arrow_Ptr, Arrow_Ptr4, CSG_Obj_List);
      Arrow_Ptr  := CSG_Union (Arrow_Ptr, Arrow_Ptr5, CSG_Obj_List);
      Grid_Ptr   := Construct_Grid (ORIGIN_3D, CSG_Obj_List, Blue_Ptr);
      Face_Ptr   := Construct_Observer (Construct_Point (0.0, 0.0, 10.0), CSG_Obj_List, Blue_Ptr);
      --  The non-CSG objects
      Cylinder_Ptr :=
        Construct_Positioned_Cylinder ("Cylinder", Construct_Point (-1.0, 0.0, -5.0), Construct_Point (-1.0, 3.0, -5.0), 2.0);
      Cylinder_Ptr.Set_Object_Material (Green_Ptr);
      Add_Object (Cylinder_Ptr, Obj_List);
      --  Add it all together
      CSG_All := CSG_Union (Grid_Ptr, Arrow_Ptr, CSG_Obj_List);
      CSG_All := CSG_Union (CSG_All, Face_Ptr, CSG_Obj_List);

      --  Lights
      Dir_Lt1 := Construct_Point_Light ("Point Light", 0.75, WHITE_RGB_Spec, Position, False);
      Add_Light (Dir_Lt1, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_CSG_Object_List (CSG_Obj_List);
      Set_CSG_Tree (CSG_All);
      Set_Light_List (Lt_List);
      --  Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Background_Color (WHITE_RGB_Spec);
      Set_Name ("Forward Ray Tracing");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/forward_ray_tracing.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;

   end Illustration_Forward_Ray_Tracing;

   procedure Illustration_Forward_Ray_Tracing_Problem is
      Cam_Ptr                                            : Camera_Ptr;
      Smp_Ptr                                            : Sampler_Ptr;
      Arrow_Ptr, Star_Ptr, Face_Ptr, CSG_All             : Object_Ptr;
      Arrow_Ptr1, Arrow_Ptr2, Arrow_Ptr3                 : Object_Ptr;
      Arrow_Ptr4, Arrow_Ptr5                             : Object_Ptr;
      Grid_Ptr                                           : Object_Ptr;
      Cylinder_Ptr                                       : Object_Ptr;
      Trn1                                               : Matrix_3D;
      Obj_List                                           : Object_List;
      CSG_Obj_List                                       : Object_List;
      Lt_List                                            : Light_List;
      Dir_Lt1                                            : Light_Ptr;
      Red_Ptr, Yellow_Ptr, Green_Ptr, Blue_Ptr, Cyan_Ptr : Material_Ptr;
      Position                                           : Point_3D;
      Look_At                                            : Point_3D;
      Up                                                 : Vector_3D;

   begin
      --  Build the world...
      Position := Construct_Point (30.0, 20.0, 100.0);
      Look_At  := Construct_Point (-1.0, 1.5, -5.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 13.0, 100.0);
      Smp_Ptr  := Construct_UnitSquare_Regular_Sampler (100, "My Sampler");

      Yellow_Ptr := Construct_Lambertian ("Bright Yellow", YELLOW_RGB_Spec, YELLOW_RGB_Spec);
      Red_Ptr    := Construct_Lambertian ("Bright Red", RED_RGB_Spec, RED_RGB_Spec);
      Green_Ptr  := Construct_Lambertian ("Bright Green", GREEN_RGB_Spec, GREEN_RGB_Spec);
      Blue_Ptr   := Construct_Lambertian ("Bright Blue", BLUE_RGB_Spec, BLUE_RGB_Spec);
      Cyan_Ptr   := Construct_Lambertian ("Bright Cyan", CYAN_RGB_Spec, CYAN_RGB_Spec);

      Star_Ptr   := Construct_Star (Construct_Point (5.0, 5.0, -5.0), CSG_Obj_List, Yellow_Ptr);
      Arrow_Ptr1 := Construct_Arrow (Construct_Point (5.0, 5.0, -5.0), Construct_Point (0.5, 2.0, -3.5), CSG_Obj_List, Red_Ptr);
      Arrow_Ptr2 := Construct_Arrow (Construct_Point (0.5, 2.0, -3.5), Construct_Point (0.0, 0.0, 9.0), CSG_Obj_List, Red_Ptr);
      Arrow_Ptr3 := Construct_Arrow (Construct_Point (5.0, 5.0, -5.0), Construct_Point (0.0, 0.0, 9.0), CSG_Obj_List, Cyan_Ptr);
      Arrow_Ptr4 := Construct_Arrow (Construct_Point (5.0, 5.0, -5.0), Construct_Point (-1.0, 3.0, -5.0), CSG_Obj_List, Cyan_Ptr);
      Arrow_Ptr5 := Construct_Arrow (Construct_Point (-1.0, 3.0, -5.0), Construct_Point (-4.0, 5.0, -5.0), CSG_Obj_List, Cyan_Ptr);
      Arrow_Ptr  := CSG_Union (Arrow_Ptr1, Arrow_Ptr2, CSG_Obj_List);
      Arrow_Ptr  := CSG_Union (Arrow_Ptr, Arrow_Ptr3, CSG_Obj_List);
      Arrow_Ptr  := CSG_Union (Arrow_Ptr, Arrow_Ptr4, CSG_Obj_List);
      Arrow_Ptr  := CSG_Union (Arrow_Ptr, Arrow_Ptr5, CSG_Obj_List);
      Grid_Ptr   := Construct_Grid (ORIGIN_3D, CSG_Obj_List, Blue_Ptr);
      Face_Ptr   := Construct_Observer (Construct_Point (0.0, 0.0, 10.0), CSG_Obj_List, Blue_Ptr);
      --  The non-CSG objects
      Cylinder_Ptr :=
        Construct_Positioned_Cylinder ("Cylinder", Construct_Point (-1.0, 2.5, -5.0), Construct_Point (-1.0, 3.0, -5.0), 0.5);
      Cylinder_Ptr.Set_Object_Material (Green_Ptr);
      Add_Object (Cylinder_Ptr, Obj_List);
      --  Add it all together
      CSG_All := CSG_Union (Grid_Ptr, Arrow_Ptr, CSG_Obj_List);
      CSG_All := CSG_Union (CSG_All, Face_Ptr, CSG_Obj_List);

      --  Lights
      Dir_Lt1 := Construct_Point_Light ("Point Light", 0.75, WHITE_RGB_Spec, Position, False);
      Add_Light (Dir_Lt1, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_CSG_Object_List (CSG_Obj_List);
      Set_CSG_Tree (CSG_All);
      Set_Light_List (Lt_List);
      --  Set_Background_Color (Construct_RGB_Spectrum (0.1, 0.1, 0.3));
      Set_Background_Color (WHITE_RGB_Spec);
      Set_Name ("Forward Ray Tracing Problem");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/forward_ray_tracing_problem.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;

   end Illustration_Forward_Ray_Tracing_Problem;

   procedure Gallery_II_Baseline_Scene1 is
      Cam_Ptr                                  : Camera_Ptr;
      Smp_Ptr                                  : Sampler_Ptr;
      Floor, LWall1, BWall1, BWall2, RWall     : Object_Ptr;
      Ceiling, CLight, WLight1, WLight2        : Object_Ptr;
      Ball, Cylinder, Block                    : Object_Ptr;
      Trn1, Trn2                               : Matrix_3D;
      Obj_List                                 : Object_List;
      Lt_List                                  : Light_List;
      Lt1, Lt2, Lt3, Lt4                       : Light_Ptr;
      Mat1, Mat2, Mat3, Mat4, Mat5, Mat6, Mat7 : Material_Ptr;
      Position                                 : Point_3D;
      Look_At                                  : Point_3D;
      Up                                       : Vector_3D;
      Col1                                     : RGB_Spectrum := WHITE_RGB_Spec;

   begin
      --  Build the world...
      Position := Construct_Point (3.0, 10.0, 40.0);
      Look_At  := Construct_Point (0.0, 10.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 15.0, 20.0);

      --  Set the scene by setting the materials
      Mat1 := Construct_Lambertian ("Dull White", WHITE_RGB_Spec, WHITE_RGB_Spec, 0.3, 0.7);
      Mat2 := Construct_Lambertian ("Dull Green", GREEN_RGB_Spec, GREEN_RGB_Spec, 0.3, 0.7);
      Mat3 := Construct_Lambertian ("Dull Red", RED_RGB_Spec, RED_RGB_Spec, 0.3, 0.7);
      Mat4 := Construct_Lambertian ("Dull Blue", BLUE_RGB_Spec, BLUE_RGB_Spec, 0.3, 0.7);
      Mat5 :=
        Construct_Lambertian
          ("Dull Brown", Construct_RGB_Spectrum (1.0, 0.7, 0.4), Construct_RGB_Spectrum (1.0, 0.7, 0.4), 0.3, 0.7);
      Mat6 := Construct_Lambertian ("Dull Yellow", YELLOW_RGB_Spec, YELLOW_RGB_Spec, 0.3, 0.7);
      Mat7 := Construct_Lambertian ("Dull Cyan", CYAN_RGB_Spec, CYAN_RGB_Spec, 0.3, 0.7);

      --  The floor
      Floor := Construct_Unit_Cube ("Floor");
      Trn1  := Construct_Scale (10.0, 0.1, 10.0);
      Floor.Transform (Trn1);
      Floor.Set_Object_Material (Mat1);
      Add_Object (Floor, Obj_List);

      --  The ceiling
      Ceiling := Construct_Unit_Cube ("Ceiling");
      Trn1    := Construct_Scale (10.0, 0.1, 10.0);
      Trn1    := Construct_Translation (0.0, 20.0, 0.0) * Trn1;
      Ceiling.Transform (Trn1);
      Ceiling.Set_Object_Material (Mat1);
      Add_Object (Ceiling, Obj_List);

      --  The left wall 1
      LWall1 := Construct_Unit_Cube ("Left Wall 1");
      Trn1   := Construct_Scale (0.1, 10.0, 7.0);
      Trn1   := Construct_Translation (-10.0, 10.0, 3.0) * Trn1;
      LWall1.Transform (Trn1);
      LWall1.Set_Object_Material (Mat4);
      Add_Object (LWall1, Obj_List);

      --  The back wall 1
      BWall1 := Construct_Unit_Cube ("Back Wall 1");
      Trn1   := Construct_Scale (5.0, 10.0, 0.1);
      Trn1   := Construct_Translation (-5.0, 10.0, -4.0) * Trn1;
      BWall1.Transform (Trn1);
      BWall1.Set_Object_Material (Mat1);
      Add_Object (BWall1, Obj_List);

      --  The left wall 2
      LWall1 := Construct_Unit_Cube ("Left Wall 2");
      Trn1   := Construct_Scale (0.1, 10.0, 3.0);
      Trn1   := Construct_Translation (0.0, 10.0, -7.0) * Trn1;
      LWall1.Transform (Trn1);
      LWall1.Set_Object_Material (Mat3);
      Add_Object (LWall1, Obj_List);

      --  The back wall 2
      BWall2 := Construct_Unit_Cube ("Back Wall 2");
      Trn1   := Construct_Scale (5.0, 10.0, 0.1);
      Trn1   := Construct_Translation (5.0, 10.0, -10.0) * Trn1;
      BWall2.Transform (Trn1);
      BWall2.Set_Object_Material (Mat1);
      Add_Object (BWall2, Obj_List);

      --  The right wall
      RWall := Construct_Unit_Cube ("Right Wall");
      Trn1  := Construct_Scale (0.1, 10.0, 10.0);
      Trn1  := Construct_Translation (10.0, 10.0, 0.0) * Trn1;
      RWall.Transform (Trn1);
      RWall.Set_Object_Material (Mat2);
      Add_Object (RWall, Obj_List);

      --  The ceiling light
      CLight := Construct_Unit_Cube ("Ceiling Light");
      Trn1   := Construct_Scale (2.0, 0.25, 2.0);
      Trn1   := Construct_Translation (0.0, 19.75, 0.0) * Trn1;
      CLight.Transform (Trn1);
      CLight.Set_Object_Material (Mat1);
      Add_Object (CLight, Obj_List);

      --  The wall light 1
      WLight1 := Construct_Unit_Sphere ("Wall Light 1");
      Trn1    := Construct_Scale (1.0, 1.0, 1.0);
      Trn1    := Construct_Translation (-10.0, 10.0, 3.0) * Trn1;
      WLight1.Transform (Trn1);
      WLight1.Set_Object_Material (Mat1);
      Add_Object (WLight1, Obj_List);

      --  The wall light 2
      WLight2 := Construct_Unit_Sphere ("Wall Light 2");
      Trn1    := Construct_Scale (1.0, 1.0, 1.0);
      Trn1    := Construct_Translation (4.0, 15.0, -10.0) * Trn1;
      WLight2.Transform (Trn1);
      WLight2.Set_Object_Material (Mat1);
      Add_Object (WLight2, Obj_List);

      --  The block
      Block := Construct_Unit_Cube ("Block");
      Trn1  := Construct_Rotate_Y (0.6);
      Trn1  := Construct_Scale (2.0, 3.0, 2.0) * Trn1;
      Trn1  := Construct_Translation (-6.0, 3.0, 6.0) * Trn1;
      Block.Transform (Trn1);
      Block.Set_Object_Material (Mat5);
      Add_Object (Block, Obj_List);

      --  The cylinder
      Cylinder := Construct_Unit_Cylinder ("Cylinder");
      Trn1     := Construct_Scale (2.0, 4.0, 2.0);
      Trn1     := Construct_Translation (4.0, 4.0, -4.0) * Trn1;
      Cylinder.Transform (Trn1);
      Cylinder.Set_Object_Material (Mat6);
      Add_Object (Cylinder, Obj_List);

      --  The Ball
      Ball := Construct_Unit_Sphere ("Ball");
      Trn1 := Construct_Scale (2.0, 2.0, 2.0);
      Trn1 := Construct_Translation (7.0, 2.0, 7.0) * Trn1;
      Ball.Transform (Trn1);
      Ball.Set_Object_Material (Mat7);
      Add_Object (Ball, Obj_List);

      --  Set the lights
      Lt1 := Construct_Point_Light ("Ceiling Light", 0.5, WHITE_RGB_Spec, Construct_Point (0.0, 19.74, 0.0), False);
      Add_Light (Lt1, Lt_List);

      Lt2 := Construct_Point_Light ("Wall Light 1", 0.9, WHITE_RGB_Spec, Construct_Point (-8.95, 10.0, 3.0), False);
      Add_Light (Lt2, Lt_List);

      Lt3 := Construct_Point_Light ("Wall Light 2", 0.9, WHITE_RGB_Spec, Construct_Point (4.0, 15.0, -8.95), False);
      Add_Light (Lt3, Lt_List);

      Lt4 := Construct_Directional_Light ("Extra Light", 0.2, WHITE_RGB_Spec, Construct_Vector (-0.5, 0.5, -0.5), False);
      Add_Light (Lt4, Lt_List);

      Set_Ambient_Light (Construct_RGB_Spectrum (0.3, 0.3, 0.3));

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.2, 0.3, 1.0));
      Set_Name ("Gallery 1 M_Dart Box");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/gallery_ii_baseline_scene1.ppm");
      Smp_Ptr := Construct_UnitSquare_Regular_Sampler (1, "My Sampler");
      Smp_Ptr.Initialize;
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;
   end Gallery_II_Baseline_Scene1;

   procedure Gallery_III_Baseline_Scene1 is
      Cam_Ptr                                  : Camera_Ptr;
      Smp_Ptr                                  : Sampler_Ptr;
      Floor, LWall1, BWall1, BWall2, RWall     : Object_Ptr;
      Ceiling, CLight, WLight1, WLight2        : Object_Ptr;
      Ball, Cylinder, Block                    : Object_Ptr;
      Trn1, Trn2                               : Matrix_3D;
      Obj_List                                 : Object_List;
      Lt_List                                  : Light_List;
      Lt1, Lt2, Lt3, Lt4                       : Light_Ptr;
      Mat1, Mat2, Mat3, Mat4, Mat5, Mat6, Mat7 : Material_Ptr;
      Position                                 : Point_3D;
      Look_At                                  : Point_3D;
      Up                                       : Vector_3D;
      Col1                                     : RGB_Spectrum := WHITE_RGB_Spec;

   begin
      --  Build the world...
      Position := Construct_Point (3.0, 10.0, 40.0);
      Look_At  := Construct_Point (0.0, 10.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 1500, 1500, 15.0, 20.0);

      --  Set the scene by setting the materials
      Mat1 := Construct_Lambertian ("Dull White", WHITE_RGB_Spec, WHITE_RGB_Spec);
      Mat2 := Construct_Lambertian ("Dull Green", GREEN_RGB_Spec, GREEN_RGB_Spec);
      Mat3 := Construct_Lambertian ("Dull Red", RED_RGB_Spec, RED_RGB_Spec);
      Mat4 := Construct_Lambertian ("Dull Blue", BLUE_RGB_Spec, BLUE_RGB_Spec);
      Mat5 := Construct_Lambertian ("Dull Brown", Construct_RGB_Spectrum (1.0, 0.7, 0.4), Construct_RGB_Spectrum (1.0, 0.7, 0.4));
      Mat6 :=
        Construct_Phong
          ("Dull Yellow", YELLOW_RGB_Spec, YELLOW_RGB_Spec, Construct_RGB_Spectrum (0.9, 0.9, 0.9), 1.0, 1.0, 1.0, 6);
      Mat7 :=
        Construct_Reflective
          ("Reflective Cyan", CYAN_RGB_Spec, CYAN_RGB_Spec, Construct_RGB_Spectrum (0.9, 0.9, 0.9), CYAN_RGB_Spec, 0.2, 0.3, 0.5,
           0.5, 50);

      --  The floor
      Floor := Construct_Unit_Cube ("Floor");
      Trn1  := Construct_Scale (10.0, 0.1, 10.0);
      Floor.Transform (Trn1);
      Floor.Set_Object_Material (Mat1);
      Add_Object (Floor, Obj_List);

      --  The ceiling
      Ceiling := Construct_Unit_Cube ("Ceiling");
      Trn1    := Construct_Scale (10.0, 0.1, 10.0);
      Trn1    := Construct_Translation (0.0, 20.0, 0.0) * Trn1;
      Ceiling.Transform (Trn1);
      Ceiling.Set_Object_Material (Mat1);
      Add_Object (Ceiling, Obj_List);

      --  The left wall 1
      LWall1 := Construct_Unit_Cube ("Left Wall 1");
      Trn1   := Construct_Scale (0.1, 10.0, 7.0);
      Trn1   := Construct_Translation (-10.0, 10.0, 3.0) * Trn1;
      LWall1.Transform (Trn1);
      LWall1.Set_Object_Material (Mat4);
      Add_Object (LWall1, Obj_List);

      --  The back wall 1
      BWall1 := Construct_Unit_Cube ("Back Wall 1");
      Trn1   := Construct_Scale (5.0, 10.0, 0.1);
      Trn1   := Construct_Translation (-5.0, 10.0, -4.0) * Trn1;
      BWall1.Transform (Trn1);
      BWall1.Set_Object_Material (Mat1);
      Add_Object (BWall1, Obj_List);

      --  The left wall 2
      LWall1 := Construct_Unit_Cube ("Left Wall 2");
      Trn1   := Construct_Scale (0.1, 10.0, 3.0);
      Trn1   := Construct_Translation (0.0, 10.0, -7.0) * Trn1;
      LWall1.Transform (Trn1);
      LWall1.Set_Object_Material (Mat3);
      Add_Object (LWall1, Obj_List);

      --  The back wall 2
      BWall2 := Construct_Unit_Cube ("Back Wall 2");
      Trn1   := Construct_Scale (5.0, 10.0, 0.1);
      Trn1   := Construct_Translation (5.0, 10.0, -10.0) * Trn1;
      BWall2.Transform (Trn1);
      BWall2.Set_Object_Material (Mat1);
      Add_Object (BWall2, Obj_List);

      --  The right wall
      RWall := Construct_Unit_Cube ("Right Wall");
      Trn1  := Construct_Scale (0.1, 10.0, 10.0);
      Trn1  := Construct_Translation (10.0, 10.0, 0.0) * Trn1;
      RWall.Transform (Trn1);
      RWall.Set_Object_Material (Mat2);
      Add_Object (RWall, Obj_List);

      --  The ceiling light
      CLight := Construct_Unit_Cube ("Ceiling Light");
      Trn1   := Construct_Scale (2.0, 0.25, 2.0);
      Trn1   := Construct_Translation (0.0, 19.75, 0.0) * Trn1;
      CLight.Transform (Trn1);
      CLight.Set_Object_Material (Mat1);
      Add_Object (CLight, Obj_List);

      --  The wall light 1
      WLight1 := Construct_Unit_Sphere ("Wall Light 1");
      Trn1    := Construct_Scale (1.0, 1.0, 1.0);
      Trn1    := Construct_Translation (-10.0, 10.0, 3.0) * Trn1;
      WLight1.Transform (Trn1);
      WLight1.Set_Object_Material (Mat1);
      Add_Object (WLight1, Obj_List);

      --  The wall light 2
      WLight2 := Construct_Unit_Sphere ("Wall Light 2");
      Trn1    := Construct_Scale (1.0, 1.0, 1.0);
      Trn1    := Construct_Translation (4.0, 15.0, -10.0) * Trn1;
      WLight2.Transform (Trn1);
      WLight2.Set_Object_Material (Mat1);
      Add_Object (WLight2, Obj_List);

      --  The block
      Block := Construct_Unit_Cube ("Block");
      Trn1  := Construct_Rotate_Y (0.6);
      Trn1  := Construct_Scale (2.0, 3.0, 2.0) * Trn1;
      Trn1  := Construct_Translation (-6.0, 3.0, 6.0) * Trn1;
      Block.Transform (Trn1);
      Block.Set_Object_Material (Mat5);
      Add_Object (Block, Obj_List);

      --  The cylinder
      Cylinder := Construct_Unit_Cylinder ("Cylinder");
      Trn1     := Construct_Scale (2.0, 4.0, 2.0);
      Trn1     := Construct_Translation (4.0, 4.0, -4.0) * Trn1;
      Cylinder.Transform (Trn1);
      Cylinder.Set_Object_Material (Mat6);
      Add_Object (Cylinder, Obj_List);

      --  The Ball
      Ball := Construct_Unit_Sphere ("Ball");
      Trn1 := Construct_Scale (2.0, 2.0, 2.0);
      Trn1 := Construct_Translation (7.0, 2.0, 7.0) * Trn1;
      Ball.Transform (Trn1);
      Ball.Set_Object_Material (Mat7);
      Add_Object (Ball, Obj_List);

      --  Set the lights
      Lt1 := Construct_Point_Light ("Ceiling Light", 0.4, WHITE_RGB_Spec, Construct_Point (0.0, 19.74, 0.0), True);
      Add_Light (Lt1, Lt_List);

      Lt2 := Construct_Point_Light ("Wall Light 1", 0.5, WHITE_RGB_Spec, Construct_Point (-8.95, 10.0, 3.0), True);
      Add_Light (Lt2, Lt_List);

      Lt3 := Construct_Point_Light ("Wall Light 2", 0.4, WHITE_RGB_Spec, Construct_Point (4.0, 15.0, -8.95), True);
      Add_Light (Lt3, Lt_List);

      Lt4 := Construct_Directional_Light ("Extra Light", 0.3, WHITE_RGB_Spec, Construct_Vector (-0.5, 0.5, -0.5), True);
      Add_Light (Lt4, Lt_List);

      Set_Ambient_Light (Construct_RGB_Spectrum (0.15, 0.15, 0.15));

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.2, 0.3, 1.0));
      Set_Name ("Gallery 2 M_Dart Box");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/gallery_iii_baseline_scene1.ppm");
      Smp_Ptr := Construct_UnitSquare_Regular_Sampler (9, "My Sampler");
      Smp_Ptr.Initialize;
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;
   end Gallery_III_Baseline_Scene1;

   procedure Gallery_II_Swirly_Spheres is
      Cam_Ptr                            : Camera_Ptr;
      Smp_Ptr                            : Sampler_Ptr;
      Sphere                             : Object_Ptr;
      Trn                                : Matrix_3D;
      Obj_List                           : Object_List;
      Lt_List                            : Light_List;
      Dir_Lt1, Dir_Lt2                   : Light_Ptr;
      Mat                                : array (0 .. 4_000) of Material_Ptr;
      A, C, X, Y, Z                      : Large_Float;
      Phi, Theta, Delta_Phi, Delta_Theta : Large_Float;
      Red, Grn, Blu                      : Small_Float;
      Position                           : Point_3D;
      Look_At                            : Point_3D;
      Up                                 : Vector_3D;
   begin
      --  Build the world...
      Position := Construct_Point (-20.0, 2.0, 20.0);
      Look_At  := Construct_Point (0.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 1024, 1024, 20.0, 20.0);

      --  Set the scene
      Phi         := 0.0;
      Delta_Phi   := Ray_2PI / 16.0;
      Delta_Theta := Ray_PI / 100.0;

      for j in 0 .. 15 loop

         Theta := Ray_2PI;
         for i in 0 .. 99 loop
            --  Determine the X, Y and Z coordinate by computing the polar coordinates back to Cartesian XYZ

            A := 0.20 * Sin (Large_Float (i) * (Ray_2PI / 40.0));
            X := 10.0 * Sin (Theta) * Cos (Phi + A);
            Y := 10.0 * Sin (Theta) * Sin (Phi + A);
            Z := 10.0 * Cos (Theta);
            C := 0.4;

            --  Add the sphere
            Trn    := Construct_Translation (X, Y, Z) * Construct_Scale (C, C, C);
            Sphere := Construct_Unit_Sphere ("Basic Sphere " & Integer'Image (i));
            Add_Object (Sphere, Obj_List);
            Sphere.Transform (Trn);
            Red               := (1.0 + Small_Float (Cos (Theta))) / 2.0;
            Grn               := (1.0 + Small_Float (Sin (Theta))) / 2.0;
            Blu               := (1.0 + Small_Float (Cos (Phi))) / 2.0;
            Mat (i + j * 200) :=
              Construct_Lambertian ("Dull Red", Construct_RGB_Spectrum (Red, Grn, Blu), Construct_RGB_Spectrum (Red, Grn, Blu));

            Sphere.Set_Object_Material (Mat (i + j * 200));

            Theta := Theta - Delta_Theta;

         end loop;

         Phi := Phi + Delta_Phi;
      end loop;

      --  Set the lights
      Dir_Lt1 := Construct_Directional_Light ("My Direct Light 1", 0.7, WHITE_RGB_Spec, Construct_Vector (0.0, 0.0, -1.0), True);
      Add_Light (Dir_Lt1, Lt_List);
      Dir_Lt2 := Construct_Directional_Light ("My Direct Light 2", 0.7, WHITE_RGB_Spec, Construct_Vector (1.0, 0.0, -1.0), True);
      Add_Light (Dir_Lt2, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.2, 0.3, 1.0));
      Set_Name ("Gallery 2 Swirly Sphere");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/gallery_ii_swirly_sphere.ppm");
      Set_Camera (Cam_Ptr);
      Smp_Ptr := Construct_UnitSquare_Random_Sampler (1, 0.2, "My Sampler");
      Smp_Ptr.Initialize;
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;

   end Gallery_II_Swirly_Spheres;

   procedure Gallery_III_Swirly_Spheres is
      Cam_Ptr                            : Camera_Ptr;
      Smp_Ptr                            : Sampler_Ptr;
      Sphere                             : Object_Ptr;
      Trn                                : Matrix_3D;
      Obj_List                           : Object_List;
      Lt_List                            : Light_List;
      Dir_Lt1, Dir_Lt2                   : Light_Ptr;
      Mat                                : array (0 .. 4_000) of Material_Ptr;
      A, C, X, Y, Z                      : Large_Float;
      Phi, Theta, Delta_Phi, Delta_Theta : Large_Float;
      Red, Grn, Blu                      : Small_Float;
      Position                           : Point_3D;
      Look_At                            : Point_3D;
      Up                                 : Vector_3D;
   begin
      --  Build the world...
      Position := Construct_Point (-20.0, 2.0, 20.0);
      Look_At  := Construct_Point (0.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 100, 100, 20.0, 20.0);

      --  Set the scene
      Phi         := 0.0;
      Delta_Phi   := Ray_2PI / 16.0;
      Delta_Theta := Ray_PI / 100.0;

      for j in 0 .. 15 loop

         Theta := Ray_2PI;
         for i in 0 .. 99 loop
            --  Determine the X, Y and Z coordinate by computing the polar coordinates back to Cartesian XYZ

            A := 0.20 * Sin (Large_Float (i) * (Ray_2PI / 40.0));
            X := 10.0 * Sin (Theta) * Cos (Phi + A);
            Y := 10.0 * Sin (Theta) * Sin (Phi + A);
            Z := 10.0 * Cos (Theta);
            C := 0.4;

            --  Add the sphere
            Trn    := Construct_Translation (X, Y, Z) * Construct_Scale (C, C, C);
            Sphere := Construct_Unit_Sphere ("Basic Sphere " & Integer'Image (i));
            Add_Object (Sphere, Obj_List);
            Sphere.Transform (Trn);
            Red               := (1.0 + Small_Float (Cos (Theta))) / 2.0;
            Grn               := (1.0 + Small_Float (Sin (Theta))) / 2.0;
            Blu               := (1.0 + Small_Float (Cos (Phi))) / 2.0;
            Mat (i + j * 200) :=
              Construct_Phong
                ("Shiny Red", Construct_RGB_Spectrum (Red, Grn, Blu), Construct_RGB_Spectrum (Red, Grn, Blu), WHITE_RGB_Spec, 1.0,
                 1.0, 1.0, 30);
            Sphere.Set_Object_Material (Mat (i + j * 200));

            Theta := Theta - Delta_Theta;

         end loop;

         Phi := Phi + Delta_Phi;
      end loop;

      --  Set the lights
      Dir_Lt1 := Construct_Directional_Light ("My Direct Light 1", 0.7, WHITE_RGB_Spec, Construct_Vector (0.0, 0.0, -1.0), True);
      Add_Light (Dir_Lt1, Lt_List);
      Dir_Lt2 := Construct_Directional_Light ("My Direct Light 2", 0.7, WHITE_RGB_Spec, Construct_Vector (1.0, 0.0, -1.0), True);
      Add_Light (Dir_Lt2, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.2, 0.3, 1.0));
      Set_Name ("Gallery 3 Swirly Sphere");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/gallery_iii_swirly_sphere.ppm");
      Set_Camera (Cam_Ptr);
      Smp_Ptr := Construct_UnitSquare_Regular_Sampler (1, "My Sampler");
      Smp_Ptr.Initialize;
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;

   end Gallery_III_Swirly_Spheres;

   procedure Gallery_II_Nautilus is
      Cam_Ptr          : Camera_Ptr;
      Smp_Ptr          : Sampler_Ptr;
      Sphere           : Object_Ptr;
      Trn              : Matrix_3D;
      Obj_List         : Object_List;
      Lt_List          : Light_List;
      Dir_Lt1, Dir_Lt2 : Light_Ptr;
      Mat              : array (1 .. 200) of Material_Ptr;
      A, B, C, X, Y    : Large_Float;
      Red, Grn, Blu    : Small_Float;
      Position         : Point_3D;
      Look_At          : Point_3D;
      Up               : Vector_3D;
   begin
      --  Build the world...
      Position := Construct_Point (10.0, 3.0, 10.0);
      Look_At  := Construct_Point (2.0, 0.0, 0.0);
      Up       := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr  := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 50.0, 20.0);

      --  Set the scene
      for i in 1 .. 200 loop
         --  Determine the X and Y coordinate and the radius of the sphere
         A := Large_Float (i) * Ray_2PI / 50.0; --  Circular position
         B := 0.5 + Large_Float (i) / 25.0; -- Circular radius
         C := 1.0 + Large_Float (i) / 200.0; -- Radius of the sphere
         X := B * Cos (A);
         Y := B * Sin (A);
         --  Add the sphere
         Trn    := Construct_Translation (X, Y, 0.0) * Construct_Scale (C, C, C);
         Sphere := Construct_Unit_Sphere ("Basic Sphere " & Integer'Image (i));
         Add_Object (Sphere, Obj_List);
         Sphere.Transform (Trn);
         Red     := Small_Float (i) / 200.0;
         Grn     := 1.0 - Red;
         Blu     := 0.3;
         Mat (i) :=
           Construct_Lambertian ("Dull Red", Construct_RGB_Spectrum (Red, Grn, Blu), Construct_RGB_Spectrum (Red, Grn, Blu));
         Sphere.Set_Object_Material (Mat (i));
      end loop;
      --  Set the lights
      Dir_Lt1 := Construct_Directional_Light ("My Direct Light 1", 0.5, WHITE_RGB_Spec, Construct_Vector (0.0, 0.0, -1.0), False);
      Add_Light (Dir_Lt1, Lt_List);
      Dir_Lt2 := Construct_Directional_Light ("My Direct Light 2", 0.5, WHITE_RGB_Spec, Construct_Vector (-1.0, 0.0, 0.0), False);
      Add_Light (Dir_Lt2, Lt_List);

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.2, 0.3, 1.0));
      Set_Name ("Gallery 1 Nautilus");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/gallery_ii_nautilus.ppm");
      Smp_Ptr := Construct_UnitSquare_Regular_Sampler (1, "My Sampler");
      Smp_Ptr.Initialize;
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;

   end Gallery_II_Nautilus;

   procedure Gallery_III_Transparency1 is
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
      Cam_Ptr := Construct_Pinhole_Camera (Position, Look_At, Up, 1_024, 1_024, 15.0, 100.0);
      Smp_Ptr := Construct_UnitSquare_Regular_Sampler (1, "My Sampler");
      Set_Max_Recursion_Level (100);

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
      Trn1       := Construct_Translation (0.0, 1.71, -2.0) * Construct_Rotate_Y (Ray_PI_div4) * Construct_Scale (3.0, 1.5, 0.2);
      Glass_Obj1.Transform (Trn1);
      Glass_Obj2 := Construct_Unit_Cube ("Cube 1");
      Trn1       := Construct_Translation (0.0, 1.51, 0.0) * Construct_Rotate_Y (Ray_PI_div4) * Construct_Scale (2.7, 1.3, 0.2);
      Glass_Obj2.Transform (Trn1);
      Glass_Obj3 := Construct_Unit_Cube ("Cube 1");
      Trn1       := Construct_Translation (0.0, 1.31, 2.0) * Construct_Rotate_Y (Ray_PI_div4) * Construct_Scale (2.3, 1.1, 0.2);
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
      Add_Object (Glass_Obj1, Obj_List);
      Add_Object (Glass_Obj2, Obj_List);
      Add_Object (Glass_Obj3, Obj_List);

      --  Add it all up
      Set_CSG_Object_List (CSG_Obj_List);
      --  Set_CSG_Tree (Glass_Obj3);

      --  Lights
      Lt1 := Construct_Point_Light ("Point Light", 0.35, WHITE_RGB_Spec, Construct_Point (0.0, 10.0, 1.0), True);
      Add_Light (Lt1, Lt_List);
      Lt2 := Construct_Point_Light ("Point Light", 0.35, WHITE_RGB_Spec, Construct_Point (20.0, 5.0, 20.0), True);
      Add_Light (Lt2, Lt_List);
      Lt3 := Construct_Point_Light ("Point Light", 0.35, WHITE_RGB_Spec, Construct_Point (20.0, 15.0, 8.0), True);
      Add_Light (Lt3, Lt_List);
      Set_Ambient_Light (Construct_RGB_Spectrum (0.2, 0.2, 0.2));

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.6, 0.6, 0.8));

      Set_Name ("Transparency Test");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/g3-t1-w0.000001-rec100.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;

   end Gallery_III_Transparency1;

   procedure Gallery_III_Transparency2 is
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
      Look_At := Construct_Point (0.0, 2.0, 0.0);
      Up      := Construct_Vector (0.0, 1.0, 0.0);
      Cam_Ptr := Construct_Pinhole_Camera (Position, Look_At, Up, 512, 512, 20.0, 100.0);
      Smp_Ptr := Construct_UnitSquare_Regular_Sampler (1, "My Sampler");
      Set_Max_Recursion_Level (1_000);
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
           Construct_RGB_Spectrum (0.99, 0.99, 0.99), Construct_RGB_Spectrum (0.9, 1.0, 0.9),
           Construct_RGB_Spectrum (0.9, 1.0, 0.9), 0.0, 0.0, 0.0, 0.1, 0.9, 100, 1.02);

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

      Glass_Obj1 := Construct_Positioned_Sphere ("Glass Sphere 1", Construct_Point (0.0, 2.5, 0.0), 1.5);
      Glass_Obj2 :=
        Construct_Positioned_Cylinder
          ("Glass Cylinder 2", Construct_Point (2.0, 1.5, -5.0), Construct_Point (3.0, 3.5, -4.0), 1.5);
      Glass_Obj3 :=
        Construct_Positioned_Cone ("Glass Cone 1", Construct_Point (-1.0, 1.5, 4.0), Construct_Point (1.0, 4.0, 4.0), 1.5);

      Glass_Obj1.Set_Object_Material (Glass);
      Glass_Obj2.Set_Object_Material (Glass);
      Glass_Obj3.Set_Object_Material (Glass);
      Add_Object (Glass_Obj1, Obj_List);
      Add_Object (Glass_Obj2, Obj_List);
      Add_Object (Glass_Obj3, Obj_List);

      --  Add it all up
      Set_CSG_Object_List (CSG_Obj_List);
      --  Set_CSG_Tree (Glass_Obj3);

      --  Lights
      Lt1 := Construct_Point_Light ("Point Light", 0.35, WHITE_RGB_Spec, Construct_Point (0.0, 10.0, 1.0), True);
      Add_Light (Lt1, Lt_List);
      Lt2 := Construct_Point_Light ("Point Light", 0.35, WHITE_RGB_Spec, Construct_Point (20.0, 5.0, 20.0), True);
      Add_Light (Lt2, Lt_List);
      Lt3 := Construct_Point_Light ("Point Light", 0.35, WHITE_RGB_Spec, Construct_Point (20.0, 15.0, 8.0), True);
      Add_Light (Lt3, Lt_List);
      Set_Ambient_Light (Construct_RGB_Spectrum (0.2, 0.2, 0.2));

      --  Build The_World
      Set_Object_List (Obj_List);
      Set_Light_List (Lt_List);
      Set_Background_Color (Construct_RGB_Spectrum (0.4, 0.4, 0.7));

      Set_Name ("Transparency Test");
      Set_FileName ("/home/mlaban/Dev/M_Dart/Images/gallery-iii-transparency2.ppm");
      Set_Camera (Cam_Ptr);
      Set_Pixel_Sampler (Cam_Ptr.all, Smp_Ptr);
      Set_Filter (Cam_Ptr.all, Construct_Box_Filter);
      Smp_Ptr.Initialize;
      Smp_Ptr.Put;

      --  Render the scene
      Render_Scene;

   end Gallery_III_Transparency2;

end Illustrations;
