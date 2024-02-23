with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Tags;               use Ada.Tags;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body Lights is

   -------------
   --  ADT Light
   -------------

   procedure Put (Lt : in Light; Msg : in String) is
   begin
      Put_Line ("BEGIN Stub " & Msg);
      Put ("Name = ");
      Put (Ada.Strings.Unbounded.To_String (Lt.Name));
      New_Line;
      Put_Line ("END " & Msg);
   end Put;

   procedure Put (Lt_List : in out Light_List; Msg : in String := "Light List ") is
      Lt_Ptr : Light_Ptr;
   begin
      Put_Line (Msg);
      Lt_Ptr := Get_First_Light (Lt_List);
      while Lt_Ptr /= null loop
         Lt_Ptr.Put ("LIGHT = ");
         Lt_Ptr := Get_Next_Light (Lt_List);
      end loop;
      Put_Line ("END Light List");
   end Put;

   procedure Add_Light (Lt_Ptr : in Light_Ptr; Lt_List : in out Light_List) is
      Lt_Elem_Ptr : Light_List_Element_Ptr;
   begin
      Lt_Elem_Ptr          := new Light_List_Element;
      Lt_Elem_Ptr.Lt_Ptr   := Lt_Ptr;
      Lt_Elem_Ptr.Next_Ptr := Lt_List.First_Ptr;
      Lt_List.First_Ptr    := Lt_Elem_Ptr;
      Lt_List.No_Of_Lights := Lt_List.No_Of_Lights + 1;
   end Add_Light;

   function Get_No_Of_Lights (Lt_List : in Light_List) return Large_Integer is (Lt_List.No_Of_Lights);

   function Get_First_Light (Lt_List : in out Light_List) return Light_Ptr is
   begin
      --  Set the current element to the first
      Lt_List.Current_Ptr := Lt_List.First_Ptr;
      return Lt_List.First_Ptr.Lt_Ptr;
   end Get_First_Light;

   function Get_Next_Light (Lt_List : in out Light_List) return Light_Ptr is
   begin
      --  Set the current element to the next
      Lt_List.Current_Ptr := Lt_List.Current_Ptr.Next_Ptr;
      if Lt_List.Current_Ptr /= null then
         return Lt_List.Current_Ptr.Lt_Ptr;
      else
         return null;
      end if;
   end Get_Next_Light;

   function Get_Light_Name (Lt : in Light'Class) return Unbounded_String is (Lt.Name);

   procedure Set_Light_Name (Lt : in out Light'Class; Name : in Unbounded_String) is
   begin
      Lt.Name := Name;
   end Set_Light_Name;

   function Get_Normal_For_Next_Light_Sample (Lt : in Light; Hp : in HitPoint) return Normal_3D is
      N : Normal_3D;
   begin
      Put_Line ("*** Illegal call of Get_Normal_To_Light");
      Lt.Put ("BAD LIGHT");
      return N;
   end Get_Normal_For_Next_Light_Sample;

   procedure Set_Light_Sampler (Lt : in out Light; Smp_Ptr : in Sampler_Ptr) is
   begin
      Lt.Light_Sampler_Ptr := Smp_Ptr;
   end Set_Light_Sampler;

   function Get_Light_Sampler (Lt : in out Light) return Sampler_Ptr is (Lt.Light_Sampler_Ptr);

   function Incident_Radiance (Lt : in Light; Hp : in HitPoint; Dir : Normal_3D) return RGB_Spectrum is
      Spec : RGB_Spectrum;
   begin
      Put_Line ("*** Illegal call of Incident_Radiance");
      Lt.Put ("BAD LIGHT");
      return Spec;
   end Incident_Radiance;

end Lights;