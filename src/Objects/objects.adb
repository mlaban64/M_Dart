with Utilities;   use Utilities;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Tags;    use Ada.Tags;

package body Objects is

   -------------
   -- ADT Object
   -------------

   procedure Put (Obj : in Object; Msg : in String := "Object ") is
   begin
      Put_Line ("BEGIN " & Msg);
      Put ("Class = ");
      Put (External_Tag (Object'Class (Obj)'Tag));
      New_Line;
      Put ("Name = ");
      Put (Ada.Strings.Unbounded.To_String (Obj.Name));
      New_Line;
      Put (Obj.Trans, "Trans = ");
      Put (Obj.Trans_Inv, "Trans_Inv = ");
      if Obj.Is_Primitive and Obj.CSG_Parent = null then
         Put ("CSG_OBJECT = False");
      else
         Put ("CSG_OBJECT = True");
      end if;
      New_Line;
      Put ("FLIP = " & Obj.Flip_Normal'Image);
      New_Line;
      Put ("INSIDE = " & Obj.Inside'Image);
      New_Line;
      Put_Line ("END " & Msg);
   end Put;

   procedure Put (Obj_List : in out Object_List; Msg : in String := "Object List ") is
      Obj : Object_Ptr;
   begin
      Put_Line (Msg);
      Obj := Get_First_Object (Obj_List);
      while Obj /= null loop
         Put (Obj.all);
         Obj := Get_Next_Object (Obj_List);
      end loop;
      Put_Line ("END Object List");
   end Put;

   procedure Add_Object (Obj : in Object_Ptr; Obj_List : in out Object_List) is
      New_Obj_Elem : Object_List_Element_Ptr;
   begin
      New_Obj_Elem           := new Object_List_Element;
      New_Obj_Elem.Obj       := Obj;
      New_Obj_Elem.Next      := Obj_List.First;
      Obj_List.First         := New_Obj_Elem;
      Obj_List.No_Of_Objects := Obj_List.No_Of_Objects + 1;
   end Add_Object;

   function Get_No_Of_Objects (Obj_List : in Object_List) return Large_Integer is (Obj_List.No_Of_Objects);

   function Get_First_Object (Obj_List : in out Object_List) return Object_Ptr is
   begin
      --  Set the current element to the first
      Obj_List.Current := Obj_List.First;
      if Obj_List.First /= null then
         return Obj_List.First.Obj;
      else
         return null;
      end if;
   end Get_First_Object;

   function Get_Next_Object (Obj_List : in out Object_List) return Object_Ptr is
   begin
      --  Set the current element to the next
      Obj_List.Current := Obj_List.Current.Next;
      if Obj_List.Current /= null then
         return Obj_List.Current.Obj;
      else
         return null;
      end if;
   end Get_Next_Object;

   function Get_Object_Name (Obj : in Object'Class) return Unbounded_String is (Obj.Name);

   procedure Set_Object_Name (Obj : in out Object'Class; Name : in Unbounded_String) is
   begin
      Obj.Name := Name;
   end Set_Object_Name;

   function Get_Object_Material (Obj : in Object) return Material_Ptr is (Obj.Material);

   procedure Set_Object_Material (Obj : in out Object; Material : in Material_Ptr) is
   begin
      Obj.Material := Material;
   end Set_Object_Material;

   function Is_Primitive (Obj : in Object'Class) return Boolean is (Obj.Is_Primitive);

   function Is_CSG_Member (Obj : in Object'Class) return Boolean is
   begin
      if Obj.Is_Primitive and Obj.CSG_Parent = null then
         return False;
      else
         if not Obj.Is_Primitive then
            Debug_Message ("*** Error: A NON-Primitive Object is intersected by a ray");
         end if;
         return True;
      end if;
   end Is_CSG_Member;

   procedure Invert_Flip_Normal (Obj : in out Object) is
   begin
      if Obj.Flip_Normal then
         Obj.Flip_Normal := False;
      else
         Obj.Flip_Normal := True;
      end if;
   end Invert_Flip_Normal;

   function Get_Flip_Normal (Obj : in Object'Class) return Boolean is (Obj.Flip_Normal);

   function Get_Object_Trans (Obj : in Object'Class) return Matrix_3D is (Obj.Trans);

   function Get_Object_Trans_Inv (Obj : in Object'Class) return Matrix_3D is (Obj.Trans_Inv);

   procedure Transform (Obj : in out Object; Mat : in Matrix_3D) is
   begin
      Obj.Trans     := Mat * Obj.Trans;
      Obj.Trans_Inv := Inverse (Obj.Trans);
   end Transform;

   function Evaluate_CSG (Obj : in out Object) return Boolean is
   begin
      Number_Of_CSG_Object_Evaluations := Number_Of_CSG_Object_Evaluations + 1;
      --  Flip the object's Inside flag. We either enter or leave the object
      if Obj.Inside then
         Obj.Inside := False;
      else
         Obj.Inside := True;
      end if;

      --  Now evaluate the parent, which should not be null
      if Obj.CSG_Parent /= null then
         return Obj.CSG_Parent.Evaluate_CSG;
      else
         Debug_Message ("*** Error: Primitive was evaluated as CSG but has no parent");
         return Obj.Inside;
      end if;
   end Evaluate_CSG;

   function Intersect (Obj : in Object; R : in Ray) return Natural is
   begin
      Put_Line ("*** Unexpected call to Object.Intersect");
      Put (Obj);
      return 0;
   end Intersect;

   function Hit (Obj : in Object; R : in Ray) return Natural is
   begin
      Put_Line ("*** Unexpected call to Object.Hit");
      Put (Obj);
      return 0;
   end Hit;

   procedure Print_Inside_Value (Obj : in Object; Level : in Large_Integer) is
   begin
      Put ("PRIMITIVE LEVEL = ");
      Put (Level);
      New_Line;
      Put (Ada.Strings.Unbounded.To_String (Obj.Name));
      New_Line;
      Put ("INSIDE = ");
      if Obj.Inside then
         Put ("TRUE");
      else
         Put ("FALSE");
      end if;
      New_Line;
   end Print_Inside_Value;

end Objects;
