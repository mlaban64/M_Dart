with Scenes;      use Scenes;
with Utilities;   use Utilities;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Tags;    use Ada.Tags;

package body Objects.CSG_Objects is

   ------------------
   -- ADT CSG_Object
   ------------------

   procedure Put (CSG_Tree : in CSG_Object; Msg : in String := "CSG Object ") is
   begin
      Put_Line (Msg);
      Object (CSG_Tree).Put;
      Put ("CSG OPS = " & CSG_Tree.Operation'Image);
      New_Line;
      Put ("INSIDE = " & CSG_Tree.Inside'Image);
      New_Line (2);
      CSG_Tree.Left_Obj.Put ("LEFT OBJ");
      New_Line;
      CSG_Tree.Right_Obj.Put ("RIGHT OBJ");
   end Put;

   function Construct_CSG_Object (Name : in String) return Object_Ptr is
      New_CSG_Ptr : Object_Ptr;
   begin
      New_CSG_Ptr              := new CSG_Object;
      New_CSG_Ptr.Is_Primitive := False;
      New_CSG_Ptr.Name         := To_Unbounded_String (Name);
      return New_CSG_Ptr;
   end Construct_CSG_Object;

   function CSG_Union (Left_Obj, Right_Obj : in out Object_Ptr; CSG_Obj_List : in out Object_List) return Object_Ptr is
      Union_Obj : CSG_Object_Ptr;
   begin
      --  Check if either child is not in use already, as this is not supported right now
      if Left_Obj.CSG_Parent /= null then
         Debug_Message ("*** ERROR: Object already has a CSG Parent. Please exit the program");
         Left_Obj.Put;
      end if;
      if Right_Obj.CSG_Parent /= null then
         Debug_Message ("*** ERROR: Object already has a CSG Parent. Please exit the program");
         Right_Obj.Put;
      end if;

      --  Create the CSG Object
      Union_Obj := CSG_Object_Ptr (Construct_CSG_Object (To_String (Left_Obj.Get_Object_Name & " + " & Right_Obj.Get_Object_Name)));
      Union_Obj.Left_Obj  := Left_Obj;
      Union_Obj.Right_Obj := Right_Obj;
      Union_Obj.Operation := UNION;

      --  Make the childs point to the CSG object
      Left_Obj.CSG_Parent  := Object_Ptr (Union_Obj);
      Right_Obj.CSG_Parent := Object_Ptr (Union_Obj);

      --  Add the child objects to the CSG Object List of the scene, in case they are primitives If not, it is assumed the child's
      --  primitives have gone through this procedure already
      if Left_Obj.Is_Primitive then
         Add_Object (Left_Obj, CSG_Obj_List);
      end if;
      if Right_Obj.Is_Primitive then
         Add_Object (Right_Obj, CSG_Obj_List);
      end if;

      return Object_Ptr (Union_Obj);
   end CSG_Union;

   function CSG_Difference (Left_Obj, Right_Obj : in out Object_Ptr; CSG_Obj_List : in out Object_List) return Object_Ptr is
      Diff_Obj : CSG_Object_Ptr;
   begin
      --  Check if either child is not in use already, as this is not supported right now
      if Left_Obj.CSG_Parent /= null or Right_Obj.CSG_Parent /= null then
         Debug_Message ("*** ERROR: Object already has a CSG Parent. Please exit the program");
      end if;

      --  Create the CSG Object
      Diff_Obj := CSG_Object_Ptr (Construct_CSG_Object (To_String (Left_Obj.Get_Object_Name & " - " & Right_Obj.Get_Object_Name)));
      Diff_Obj.Left_Obj  := Left_Obj;
      Diff_Obj.Right_Obj := Right_Obj;
      --  Flip the normals for the right hand side
      Right_Obj.Invert_Flip_Normal;
      Diff_Obj.Operation := DIFFERENCE;

      --  Make the childs point to the CSG object
      Left_Obj.CSG_Parent  := Object_Ptr (Diff_Obj);
      Right_Obj.CSG_Parent := Object_Ptr (Diff_Obj);

      --  Add the child objects to the CSG Object List of the scene
      if Left_Obj.Is_Primitive then
         Add_Object (Left_Obj, CSG_Obj_List);
      end if;
      if Right_Obj.Is_Primitive then
         Add_Object (Right_Obj, CSG_Obj_List);
      end if;

      return Object_Ptr (Diff_Obj);
   end CSG_Difference;

   function CSG_Intersection (Left_Obj, Right_Obj : in out Object_Ptr; CSG_Obj_List : in out Object_List) return Object_Ptr is
      Isect_Obj : CSG_Object_Ptr;
   begin
      --  Check if either child is not in use already, as this is not supported right now
      if Left_Obj.CSG_Parent /= null or Right_Obj.CSG_Parent /= null then
         Debug_Message ("*** ERROR: Object already has a CSG Parent. Please exit the program");
      end if;

      --  Create the CSG Object
      Isect_Obj := CSG_Object_Ptr (Construct_CSG_Object (To_String (Left_Obj.Get_Object_Name & " * " & Right_Obj.Get_Object_Name)));
      Isect_Obj.Left_Obj  := Left_Obj;
      Isect_Obj.Right_Obj := Right_Obj;
      Isect_Obj.Operation := INTERSECTION;

      --  Make the childs point to the CSG object
      Left_Obj.CSG_Parent  := Object_Ptr (Isect_Obj);
      Right_Obj.CSG_Parent := Object_Ptr (Isect_Obj);

      --  Add the child objects to the CSG Object List of the scene
      if Left_Obj.Is_Primitive then
         Add_Object (Left_Obj, CSG_Obj_List);
      end if;
      if Right_Obj.Is_Primitive then
         Add_Object (Right_Obj, CSG_Obj_List);
      end if;

      return Object_Ptr (Isect_Obj);
   end CSG_Intersection;

   overriding procedure Invert_Flip_Normal (Obj : in out CSG_Object) is
   begin
      if Obj.Flip_Normal then
         Obj.Flip_Normal := False;
      else
         Obj.Flip_Normal := True;
      end if;
      Obj.Left_Obj.Invert_Flip_Normal;
      Obj.Right_Obj.Invert_Flip_Normal;
   end Invert_Flip_Normal;

   overriding function Get_Object_Material (Obj : in CSG_Object) return Material_Ptr is
   begin
      --  if the CSG object has its own material, return it
      if Obj.Material /= null then
         return (Obj.Material);
      else --  else recursively travers the left child
         return Obj.Left_Obj.Get_Object_Material;
      end if;
   end Get_Object_Material;

   overriding procedure Set_Object_Material (Obj : in out CSG_Object; Material : in Material_Ptr) is
   begin
      Obj.Material := Material;
      Obj.Left_Obj.Set_Object_Material (Material);
      Obj.Right_Obj.Set_Object_Material (Material);
   end Set_Object_Material;

   overriding procedure Transform (Obj : in out CSG_Object; Mat : in Matrix_3D) is
   begin
      Obj.Trans     := Mat * Obj.Trans;
      Obj.Trans_Inv := Inverse (Obj.Trans);
      --  Now traverse this CSG object and apply the same transformation to its children
      Obj.Left_Obj.Transform (Mat);
      Obj.Right_Obj.Transform (Mat);
   end Transform;

   overriding function Evaluate_CSG (Obj : in out CSG_Object) return Boolean is
      New_Inside, Old_Inside : Boolean;
   begin
      Number_Of_CSG_Object_Evaluations := Number_Of_CSG_Object_Evaluations + 1;

      --  Look at the operation and evaluate this node accordingly

      --  Get the current status of the object
      Old_Inside := Obj.Inside;

      case Obj.Operation is

         when UNION => --  If we're inside either left or right child, we're inside the UNION
            if Obj.Left_Obj.Inside or Obj.Right_Obj.Inside then
               New_Inside := True;
            else --  We're outside the UNION
               New_Inside := False;
            end if;

         when DIFFERENCE => --  If we're inside the left and outside the right child, we're inside the DIFFERENCE
            if Obj.Left_Obj.Inside and not Obj.Right_Obj.Inside then
               New_Inside := True;
            else --  We're outside the DIFFERENCE
               New_Inside := False;
            end if;

         when INTERSECTION => --  If we're inside the left and the right child, we're inside the INTERSECTION
            if Obj.Left_Obj.Inside and Obj.Right_Obj.Inside then
               New_Inside := True;
            else --  We're outside the INTERSECTION
               New_Inside := False;
            end if;

      end case;

      --  Did we change the Inside Flag of the object? If so, toggle the object's flag and evaluate upwards
--        if Obj.Inside /= New_Inside then
--
--           Obj.Inside := New_Inside;
--
--           --  Now evaluate the parent, if any. If none, return this object's Inside flag
--           if Obj.CSG_Parent /= null then
--              return Obj.CSG_Parent.Evaluate_CSG;
--           end if;
--
--        end if;
      Obj.Inside := New_Inside;
      if Obj.CSG_Parent /= null then
         return Obj.CSG_Parent.Evaluate_CSG;
      else
         --  Return True if the Root CSG object flipped Inside
         return not (New_Inside = Old_Inside);
      end if;

   end Evaluate_CSG;

   procedure Print_Inside_Value (CSG_Obj : in CSG_Object; Level : in Large_Integer) is
   begin
      Put ("LEVEL = ");
      Put (Level);
      New_Line;
      Put (Ada.Strings.Unbounded.To_String (CSG_Obj.Name));
      New_Line;
      Put ("INSIDE = ");
      if CSG_Obj.Inside then
         Put ("TRUE");
      else
         Put ("FALSE");
      end if;
      New_Line;
      Print_Inside_Value (CSG_Obj.Left_Obj.all, Level + 1);
      Print_Inside_Value (CSG_Obj.Right_Obj.all, Level + 1);
   end Print_Inside_Value;

end Objects.CSG_Objects;
