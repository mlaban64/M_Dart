with Scenes;                    use Scenes;
with Objects;                   use Objects;
with Objects.Unit_Cubes;        use Objects.Unit_Cubes;
with Objects.Unit_Spheres;      use Objects.Unit_Spheres;
with Objects.Unit_Cylinders;    use Objects.Unit_Cylinders;
with Objects.Unit_Cones;        use Objects.Unit_Cones;
with Objects.CSG_Objects;       use Objects.CSG_Objects;
with Spectra;                   use Spectra;
with Materials;                 use Materials;
with Materials.Lambertian;      use Materials.Lambertian;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Small_Float_Functions;     use Small_Float_Functions;
with Normal_Float_Functions;    use Normal_Float_Functions;
with Large_Float_Functions;     use Large_Float_Functions;

package body Objects.Compounds is

   function Construct_Arrow
     (From, To : in Point_3D; CSG_Obj_List : in out Object_List; Mat_Ptr : in Material_Ptr) return Object_Ptr
   is
      Shaft_Ptr, Head_Ptr, Arrow_Ptr : Object_Ptr;
      Radius                         : Large_Float := 0.08;
      Head_Lengh                     : Large_Float := 0.8;
      Cyl_To                         : Point_3D;
   begin
      --  Construct an arrow with an arrow-head lenght of 0.5. The tip of the arrow-head is the end point (To) of the arrow, so the
      --  cylinder must be of the length (From - To) - Head_Lengh
      Cyl_To    := To + Head_Lengh * Normalize (From - To);
      Shaft_Ptr := Construct_Positioned_Cylinder ("Shaft", From, Cyl_To, Radius);
      Head_Ptr  := Construct_Positioned_Cone ("Head", Cyl_To, To, Radius * 3.0);
      Arrow_Ptr := CSG_Union (Shaft_Ptr, Head_Ptr, CSG_Obj_List);
      Arrow_Ptr.Set_Object_Material (Mat_Ptr);
      return Arrow_Ptr;
   end Construct_Arrow;

   function Construct_Double_Arrow
     (From, To : in Point_3D; CSG_Obj_List : in out Object_List; Mat_Ptr : in Material_Ptr) return Object_Ptr
   is
      Shaft_Ptr, Head1_Ptr, Head2_Ptr, Arrow_Ptr : Object_Ptr;
      Radius                                     : Large_Float := 0.08;
      Head_Lengh                                 : Large_Float := 0.8;
      Cyl_From, Cyl_To                           : Point_3D;
   begin
      --  Construct an arrow with an arrow-head lenght of 0.5. The tip of the arrow-head is the end point (To) of the arrow, so the
      --  cylinder must be of the length (From - To) - 2 * Head_Lengh
      Cyl_From  := From + Head_Lengh * Normalize (To - From);
      Cyl_To    := To + Head_Lengh * Normalize (From - To);
      Shaft_Ptr := Construct_Positioned_Cylinder ("Shaft", Cyl_From, Cyl_To, Radius);
      Head1_Ptr := Construct_Positioned_Cone ("Head1", Cyl_To, To, Radius * 3.0);
      Head2_Ptr := Construct_Positioned_Cone ("Head2", Cyl_From, From, Radius * 3.0);
      Arrow_Ptr := CSG_Union (Shaft_Ptr, Head1_Ptr, CSG_Obj_List);
      Arrow_Ptr := CSG_Union (Arrow_Ptr, Head2_Ptr, CSG_Obj_List);
      Arrow_Ptr.Set_Object_Material (Mat_Ptr);
      return Arrow_Ptr;
   end Construct_Double_Arrow;

   function Construct_Star (Pos : in Point_3D; CSG_Obj_List : in out Object_List; Mat_Ptr : in Material_Ptr) return Object_Ptr is
      Star_Ptr                           : Object_Ptr;
      o1, o2, o3, o4, o5, o6, o7, o8, o9 : Object_Ptr;
      u1, u2, u3, u4, u5, u6, u7         : Object_Ptr;
   begin
      o1 := Construct_Positioned_Cone ("Point 1", Pos, Pos + Construct_Vector (0.0, 1.0, 0.0), 0.25);
      o2 := Construct_Positioned_Cone ("Point 2", Pos, Pos + Construct_Vector (0.0, -1.0, 0.0), 0.25);
      o3 := Construct_Positioned_Cone ("Point 3", Pos, Pos + Construct_Vector (1.0, 0.0, 0.0), 0.25);
      o4 := Construct_Positioned_Cone ("Point 4", Pos, Pos + Construct_Vector (-1.0, 0.0, 0.0), 0.25);
      o5 := Construct_Positioned_Cone ("Point 5", Pos, Pos + Construct_Vector (0.7, 0.7, 0.0), 0.25);
      o6 := Construct_Positioned_Cone ("Point 6", Pos, Pos + Construct_Vector (0.7, -0.7, 0.0), 0.25);
      o7 := Construct_Positioned_Cone ("Point 7", Pos, Pos + Construct_Vector (-0.7, -0.7, 0.0), 0.25);
      o8 := Construct_Positioned_Cone ("Point 8", Pos, Pos + Construct_Vector (-0.7, 0.7, 0.0), 0.25);
      u1 := CSG_Union (o1, o2, CSG_Obj_List);
      u2 := CSG_Union (o3, o4, CSG_Obj_List);
      u3 := CSG_Union (o5, o6, CSG_Obj_List);
      u4 := CSG_Union (o7, o8, CSG_Obj_List);
      u5 := CSG_Union (u1, u2, CSG_Obj_List);
      u6 := CSG_Union (u3, u4, CSG_Obj_List);
      u7 := CSG_Union (u5, u6, CSG_Obj_List);

      --  The centre sphere
      o9       := Construct_Positioned_Sphere ("Star", Pos, 0.3);
      Star_Ptr := CSG_Union (o9, u7, CSG_Obj_List);
      Star_Ptr.Set_Object_Material (Mat_Ptr);
      return Star_Ptr;
   end Construct_Star;

   function Construct_Grid (To : in Point_3D; CSG_Obj_List : in out Object_List; Mat_Ptr : Material_Ptr) return Object_Ptr is
      Grid_Ptr       : Object_Ptr;
      c1, c2, c3     : Object_Ptr;
      u1, u2, u3, u4 : Object_Ptr;
      E1, E2         : Vector_3D;
      Radius         : Large_Float := 0.05;
   begin

      --  1st vertical bar
      E1 := Construct_Vector (-2.0, -2.0, 0.0);
      E2 := Construct_Vector (-2.0, 2.0, 0.0);
      c1 := Construct_Positioned_Cylinder ("V1", To + E1, To + E2, Radius);
      c2 := Construct_Positioned_Sphere ("E1", To + E1, Radius);
      c3 := Construct_Positioned_Sphere ("E2", To + E2, Radius);
      u1 := CSG_Union (c1, c2, CSG_Obj_List);
      u2 := CSG_Union (u1, c3, CSG_Obj_List);
      --  2nd vertical bar
      E1 := Construct_Vector (-1.0, -2.0, 0.0);
      E2 := Construct_Vector (-1.0, 2.0, 0.0);
      c1 := Construct_Positioned_Cylinder ("V1", To + E1, To + E2, Radius);
      --  Add it to the tree
      Grid_Ptr := CSG_Union (u2, c1, CSG_Obj_List);

      --  3rd vertical bar
      E1 := Construct_Vector (0.0, -2.0, 0.0);
      E2 := Construct_Vector (0.0, 2.0, 0.0);
      c1 := Construct_Positioned_Cylinder ("V1", To + E1, To + E2, Radius);
      --  Add it to the tree
      Grid_Ptr := CSG_Union (Grid_Ptr, c1, CSG_Obj_List);

      --  4th vertical bar
      E1 := Construct_Vector (1.0, -2.0, 0.0);
      E2 := Construct_Vector (1.0, 2.0, 0.0);
      c1 := Construct_Positioned_Cylinder ("V1", To + E1, To + E2, Radius);
      --  Add it to the tree
      Grid_Ptr := CSG_Union (Grid_Ptr, c1, CSG_Obj_List);

      --  5th vertical bar
      E1 := Construct_Vector (2.0, -2.0, 0.0);
      E2 := Construct_Vector (2.0, 2.0, 0.0);
      c1 := Construct_Positioned_Cylinder ("V1", To + E1, To + E2, Radius);
      c2 := Construct_Positioned_Sphere ("E1", To + E1, Radius);
      c3 := Construct_Positioned_Sphere ("E2", To + E2, Radius);
      u3 := CSG_Union (c1, c2, CSG_Obj_List);
      u4 := CSG_Union (u3, c3, CSG_Obj_List);
      --  Add it to the tree
      Grid_Ptr := CSG_Union (Grid_Ptr, u4, CSG_Obj_List);

      --  1st horizontal bar
      E1 := Construct_Vector (-2.0, -2.0, 0.0);
      E2 := Construct_Vector (2.0, -2.0, 0.0);
      c1 := Construct_Positioned_Cylinder ("V1", To + E1, To + E2, Radius);
      --  Add it to the tree
      Grid_Ptr := CSG_Union (Grid_Ptr, c1, CSG_Obj_List);

      --  2nd horizontal bar
      E1 := Construct_Vector (-2.0, -1.0, 0.0);
      E2 := Construct_Vector (2.0, -1.0, 0.0);
      c1 := Construct_Positioned_Cylinder ("V1", To + E1, To + E2, Radius);
      --  Add it to the tree
      Grid_Ptr := CSG_Union (Grid_Ptr, c1, CSG_Obj_List);

      --  3rd horizontal bar
      E1 := Construct_Vector (-2.0, 0.0, 0.0);
      E2 := Construct_Vector (2.0, 0.0, 0.0);
      c1 := Construct_Positioned_Cylinder ("V1", To + E1, To + E2, Radius);
      --  Add it to the tree
      Grid_Ptr := CSG_Union (Grid_Ptr, c1, CSG_Obj_List);

      --  4th horizontal bar
      E1 := Construct_Vector (-2.0, 1.0, 0.0);
      E2 := Construct_Vector (2.0, 1.0, 0.0);
      c1 := Construct_Positioned_Cylinder ("V1", To + E1, To + E2, Radius);
      --  Add it to the tree
      Grid_Ptr := CSG_Union (Grid_Ptr, c1, CSG_Obj_List);

      --  5th horizontal bar
      E1 := Construct_Vector (-2.0, 2.0, 0.0);
      E2 := Construct_Vector (2.0, 2.0, 0.0);
      c1 := Construct_Positioned_Cylinder ("V1", To + E1, To + E2, Radius);
      --  Add it to the tree
      Grid_Ptr := CSG_Union (Grid_Ptr, c1, CSG_Obj_List);

      Grid_Ptr.Set_Object_Material (Mat_Ptr);

      return Grid_Ptr;

   end Construct_Grid;

   function Construct_Observer (To : in Point_3D; CSG_Obj_List : in out Object_List; Mat_Ptr : in Material_Ptr) return Object_Ptr
   is
      Face_Ptr, Eye1_Ptr, Eye2_Ptr : Object_Ptr;
      Offset                       : Point_3D;
      White_Ptr                    : Material_Ptr;
   begin
      --  The face
      White_Ptr := Construct_Lambertian ("Bright White", WHITE_RGB_Spec, WHITE_RGB_Spec);
      Face_Ptr  := Construct_Positioned_Sphere ("Face", To, 1.0);
      Face_Ptr.Set_Object_Material (Mat_Ptr);

      --  The 1st eye
      Offset   := To + Construct_Vector (0.3, 0.4, 0.5);
      Eye1_Ptr := Construct_Positioned_Sphere ("Eye1", Offset, 0.4);
      Eye1_Ptr.Set_Object_Material (White_Ptr);
      --  Add it to the tree
      Face_Ptr := CSG_Union (Face_Ptr, Eye1_Ptr, CSG_Obj_List);

      --  The 2nd eye
      Offset   := To + Construct_Vector (-0.3, 0.4, 0.5);
      Eye2_Ptr := Construct_Positioned_Sphere ("Eye2", Offset, 0.4);
      Eye2_Ptr.Set_Object_Material (White_Ptr);
      --  Add it to the tree
      Face_Ptr := CSG_Union (Face_Ptr, Eye2_Ptr, CSG_Obj_List);

      return Face_Ptr;
   end Construct_Observer;

   function Construct_Pawn (Pos : in Point_3D; CSG_Obj_List : in out Object_List; Mat_Ptr : in Material_Ptr) return Object_Ptr is
      P1, P2, P3, P4, P5, P6, Pawn : Object_Ptr;
   begin
      P1   := Construct_Positioned_Cone ("Base", Construct_Point (0.0, 0.2, 0.0), Construct_Point (0.0, 1.0, 0.0), 0.6);
      P2   := Construct_Positioned_Cylinder ("Plate", Construct_Point (0.0, 0.8, 0.0), Construct_Point (0.0, 1.0, 0.0), 0.35);
      P3   := CSG_Union (P1, P2, CSG_Obj_List);
      P4   := Construct_Positioned_Cone ("Base2", Construct_Point (0.0, 1.0, 0.0), Construct_Point (0.0, 1.8, 0.0), 0.25);
      P5   := CSG_Union (P3, P4, CSG_Obj_List);
      P6   := Construct_Positioned_Sphere ("Head", Construct_Point (0.0, 1.7, 0.0), 0.3);
      Pawn := CSG_Union (P5, P6, CSG_Obj_List);
      Pawn.Transform (Construct_Translation (Get_X (Pos), Get_Y (Pos), Get_Z (Pos)));
      Pawn.Set_Object_Material (Mat_Ptr);
      return Pawn;
   end Construct_Pawn;

   function Construct_Rook (Pos : in Point_3D; CSG_Obj_List : in out Object_List; Mat_Ptr : in Material_Ptr) return Object_Ptr is
      P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, Rook : Object_Ptr;
      Trn                                                               : Matrix_3D;
   begin
      P1  := Construct_Positioned_Cone ("Base", Construct_Point (0.0, 0.2, 0.0), Construct_Point (0.0, 1.0, 0.0), 0.8);
      P2  := Construct_Positioned_Cylinder ("Plate", Construct_Point (0.0, 0.8, 0.0), Construct_Point (0.0, 1.0, 0.0), 0.4);
      P3  := CSG_Union (P1, P2, CSG_Obj_List);
      P4  := Construct_Positioned_Cylinder ("Base2", Construct_Point (0.0, 1.0, 0.0), Construct_Point (0.0, 1.8, 0.0), 0.3);
      P5  := CSG_Union (P3, P4, CSG_Obj_List);
      P6  := Construct_Positioned_Cylinder ("Head", Construct_Point (0.0, 1.8, 0.0), Construct_Point (0.0, 2.4, 0.0), 0.5);
      P7  := CSG_Union (P5, P6, CSG_Obj_List);
      P8  := Construct_Positioned_Cylinder ("Hole", Construct_Point (0.0, 2.0, 0.0), Construct_Point (0.0, 2.5, 0.0), 0.3);
      P9  := CSG_Difference (P7, P8, CSG_Obj_List);
      P10 := Construct_Unit_Cube ("Cantle");
      P10.Transform (Construct_Translation (0.0, 2.31, 0.0) * Construct_Scale (1.0, 0.14, 0.14));
      P11 := CSG_Difference (P9, P10, CSG_Obj_List);
      P12 := Construct_Unit_Cube ("Cantle");
      Trn := Construct_Translation (0.0, 2.31, 0.0) * Construct_Rotate_Y (Ray_2PI / 6.0) * Construct_Scale (1.0, 0.14, 0.14);
      P12.Transform (Trn);
      P13 := CSG_Difference (P11, P12, CSG_Obj_List);
      P14 := Construct_Unit_Cube ("Cantle");
      Trn := Construct_Translation (0.0, 2.31, 0.0) * Construct_Rotate_Y (Ray_2PI / 3.0) * Construct_Scale (1.0, 0.14, 0.14);
      P14.Transform (Trn);
      Rook := CSG_Difference (P13, P14, CSG_Obj_List);
      Rook.Transform (Construct_Translation (Get_X (Pos), Get_Y (Pos), Get_Z (Pos)));
      Rook.Set_Object_Material (Mat_Ptr);
      return Rook;
   end Construct_Rook;

   function Construct_Bishop (Pos : in Point_3D; CSG_Obj_List : in out Object_List; Mat_Ptr : in Material_Ptr) return Object_Ptr is
      P1, P2, P3, P4, P5, P6, P7, P8, Bishop : Object_Ptr;
      Trn                                    : Matrix_3D;
   begin
      P1  := Construct_Positioned_Cone ("Base", Construct_Point (0.0, 0.2, 0.0), Construct_Point (0.0, 1.0, 0.0), 0.8);
      P2  := Construct_Positioned_Cylinder ("Plate", Construct_Point (0.0, 0.8, 0.0), Construct_Point (0.0, 1.0, 0.0), 0.4);
      P3  := CSG_Union (P1, P2, CSG_Obj_List);
      P4  := Construct_Positioned_Cone ("Base2", Construct_Point (0.0, 1.0, 0.0), Construct_Point (0.0, 2.6, 0.0), 0.35);
      P5  := CSG_Union (P3, P4, CSG_Obj_List);
      P6  := Construct_Positioned_Sphere ("Head", Construct_Point (0.0, 2.5, 0.0), 0.4);
      P7  := CSG_Union (P5, P6, CSG_Obj_List);
      P8  := Construct_Unit_Cube ("Slice");
      Trn := Construct_Translation (0.0, 2.6, 0.2) * Construct_Rotate_X (Ray_2PI / 6.0) * Construct_Scale (0.5, 0.5, 0.08);
      P8.Transform (Trn);
      Bishop := CSG_Difference (P7, P8, CSG_Obj_List);
      Bishop.Transform (Construct_Translation (Get_X (Pos), Get_Y (Pos), Get_Z (Pos)));
      Bishop.Set_Object_Material (Mat_Ptr);
      return Bishop;
   end Construct_Bishop;

   function Construct_Knight (Pos : in Point_3D; CSG_Obj_List : in out Object_List; Mat_Ptr : in Material_Ptr) return Object_Ptr is
      P1, P2, P3, P4, P5, P6, P7, P8, Knight : Object_Ptr;
      Trn                                    : Matrix_3D;
   begin
      P1  := Construct_Positioned_Cone ("Base", Construct_Point (0.0, 0.2, 0.0), Construct_Point (0.0, 1.0, 0.0), 0.8);
      P2  := Construct_Positioned_Cylinder ("Plate", Construct_Point (0.0, 0.8, 0.0), Construct_Point (0.0, 1.0, 0.0), 0.4);
      P3  := CSG_Union (P1, P2, CSG_Obj_List);
      P4  := Construct_Positioned_Cone ("Base2", Construct_Point (0.0, 1.0, 0.0), Construct_Point (0.0, 2.6, 0.0), 0.35);
      P5  := CSG_Union (P3, P4, CSG_Obj_List);
      P6  := Construct_Positioned_Cylinder ("Head", Construct_Point (-0.2, 2.3, 0.0), Construct_Point (0.2, 2.3, 0.0), 0.6);
      P7  := CSG_Union (P5, P6, CSG_Obj_List);
      P8  := Construct_Unit_Cube ("Slice");
      Trn := Construct_Translation (0.0, 2.0, 0.75) * Construct_Rotate_X (Ray_2PI / 6.0) * Construct_Scale (0.6, 0.6, 0.6);
      P8.Transform (Trn);
      Knight := CSG_Difference (P7, P8, CSG_Obj_List);
      Knight.Transform (Construct_Translation (Get_X (Pos), Get_Y (Pos), Get_Z (Pos)));
      Knight.Set_Object_Material (Mat_Ptr);
      return Knight;
   end Construct_Knight;

   function Construct_Queen (Pos : in Point_3D; CSG_Obj_List : in out Object_List; Mat_Ptr : in Material_Ptr) return Object_Ptr is
      P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12       : Object_Ptr;
      P13, P14, P15, P16, P17, P18, P19, P20, P21, P22, Queen : Object_Ptr;
      Trn                                                     : Matrix_3D;
   begin
      P1  := Construct_Positioned_Cone ("Base", Construct_Point (0.0, 0.2, 0.0), Construct_Point (0.0, 1.0, 0.0), 0.8);
      P2  := Construct_Positioned_Cylinder ("Plate1", Construct_Point (0.0, 0.8, 0.0), Construct_Point (0.0, 0.9, 0.0), 0.4);
      P3  := CSG_Union (P1, P2, CSG_Obj_List);
      P4  := Construct_Positioned_Cylinder ("Plate2", Construct_Point (0.0, 1.0, 0.0), Construct_Point (0.0, 1.1, 0.0), 0.4);
      P5  := CSG_Union (P3, P4, CSG_Obj_List);
      P6  := Construct_Positioned_Cone ("Base2", Construct_Point (0.0, 1.0, 0.0), Construct_Point (0.0, 2.6, 0.0), 0.35);
      P7  := CSG_Union (P5, P6, CSG_Obj_List);
      P8  := Construct_Positioned_Sphere ("Head1", Construct_Point (0.0, 2.5, 0.0), 0.6);
      P9  := CSG_Union (P7, P8, CSG_Obj_List);
      P10 := Construct_Positioned_Sphere ("Head2", Construct_Point (0.0, 3.3, 0.0), 0.8);
      P11 := CSG_Difference (P9, P10, CSG_Obj_List);
      P12 := Construct_Positioned_Sphere ("Crown1", Construct_Point (0.0, 2.8, 0.6), 0.15);
      P13 := CSG_Union (P12, P11, CSG_Obj_List);
      P14 := Construct_Positioned_Sphere ("Crown2", Construct_Point (0.0, 2.8, 0.6), 0.15);
      Trn := Construct_Rotate_Y (1.0 * Ray_2PI / 6.0);
      P14.Transform (Trn);
      P15 := CSG_Union (P13, P14, CSG_Obj_List);
      P16 := Construct_Positioned_Sphere ("Crown3", Construct_Point (0.0, 2.8, 0.6), 0.15);
      Trn := Construct_Rotate_Y (2.0 * Ray_2PI / 6.0);
      P16.Transform (Trn);
      P17 := CSG_Union (P15, P16, CSG_Obj_List);
      P18 := Construct_Positioned_Sphere ("Crown4", Construct_Point (0.0, 2.8, 0.6), 0.15);
      Trn := Construct_Rotate_Y (3.0 * Ray_2PI / 6.0);
      P18.Transform (Trn);
      P19 := CSG_Union (P17, P18, CSG_Obj_List);
      P20 := Construct_Positioned_Sphere ("Crown5", Construct_Point (0.0, 2.8, 0.6), 0.15);
      Trn := Construct_Rotate_Y (4.0 * Ray_2PI / 6.0);
      P20.Transform (Trn);
      P21 := CSG_Union (P19, P20, CSG_Obj_List);
      P22 := Construct_Positioned_Sphere ("Crown6", Construct_Point (0.0, 2.8, 0.6), 0.15);
      Trn := Construct_Rotate_Y (5.0 * Ray_2PI / 6.0);
      P22.Transform (Trn);

      Queen := CSG_Union (P21, P22, CSG_Obj_List);
      Queen.Transform (Construct_Translation (Get_X (Pos), Get_Y (Pos), Get_Z (Pos)));
      Queen.Set_Object_Material (Mat_Ptr);
      return Queen;
   end Construct_Queen;

   function Construct_King (Pos : in Point_3D; CSG_Obj_List : in out Object_List; Mat_Ptr : in Material_Ptr) return Object_Ptr is
      P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12 : Object_Ptr;
      P13, P14, King                                    : Object_Ptr;
      Trn                                               : Matrix_3D;
   begin
      P1  := Construct_Positioned_Cone ("Base", Construct_Point (0.0, 0.2, 0.0), Construct_Point (0.0, 1.0, 0.0), 0.8);
      P2  := Construct_Positioned_Cylinder ("Plate1", Construct_Point (0.0, 0.8, 0.0), Construct_Point (0.0, 0.9, 0.0), 0.4);
      P3  := CSG_Union (P1, P2, CSG_Obj_List);
      P4  := Construct_Positioned_Cylinder ("Plate2", Construct_Point (0.0, 1.0, 0.0), Construct_Point (0.0, 1.1, 0.0), 0.4);
      P5  := CSG_Union (P3, P4, CSG_Obj_List);
      P6  := Construct_Positioned_Cone ("Base2", Construct_Point (0.0, 1.0, 0.0), Construct_Point (0.0, 2.6, 0.0), 0.35);
      P7  := CSG_Union (P5, P6, CSG_Obj_List);
      P8  := Construct_Positioned_Cone ("Head1", Construct_Point (0.0, 3.0, 0.0), Construct_Point (0.0, 2.0, 0.0), 0.6);
      P9  := CSG_Union (P7, P8, CSG_Obj_List);
      P10 := Construct_Positioned_Cone ("Head2", Construct_Point (0.0, 3.1, 0.0), Construct_Point (0.0, 2.5, 0.0), 0.6);
      P11 := CSG_Difference (P9, P10, CSG_Obj_List);

      P12 := Construct_Unit_Cube ("Crown1");
      Trn := Construct_Translation (0.0, 3.1, 0.0) * Construct_Scale (0.15, 0.7, 0.10);
      P12.Transform (Trn);
      P13 := CSG_Union (P12, P11, CSG_Obj_List);

      P14 := Construct_Unit_Cube ("Crown2");
      Trn := Construct_Translation (0.0, 3.4, 0.0) * Construct_Scale (0.5, 0.10, 0.10);
      P14.Transform (Trn);

      King := CSG_Union (P13, P14, CSG_Obj_List);
      King.Transform (Construct_Translation (Get_X (Pos), Get_Y (Pos), Get_Z (Pos)));
      King.Set_Object_Material (Mat_Ptr);
      return King;
   end Construct_King;

end Objects.Compounds;
