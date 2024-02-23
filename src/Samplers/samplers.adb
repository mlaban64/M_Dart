with Core_Types;             use Core_Types;
with Objects;                use Objects;
with HitPoints;              use HitPoints;
with ShadePoints;            use ShadePoints;
with Materials;              use Materials;
with Scenes;                 use Scenes;
with Utilities;              use Utilities;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;

with Ada.Tags; use Ada.Tags;

package body Samplers is

   --------------
   -- ADT Sampler
   --------------
   procedure Put (Smp : in Sampler'Class; Msg : in String := "Sampler = ") is
   begin
      Put_Line ("BEGIN " & Msg);
      Put ("Class = ");
      Put (External_Tag (Smp'Tag));
      New_Line;
      Put ("Name = ");
      Put (Ada.Strings.Unbounded.To_String (Smp.Name));
      New_Line;
      Put ("Number of Samples = ");
      Put (Smp.No_Of_Samples);
      New_Line;
      Put_Line ("END " & Msg);
   end Put;

   procedure Initialize (Smp : in out Sampler) is
   begin
      Debug_Message ("*** Warning: Initializing a Root Sampler");
   end Initialize;

   function Get_No_Of_Samples (Smp : in Sampler) return Natural is (Smp.No_Of_Samples);

   function Get_Next_Sample (Smp : in out Sampler) return Point_2D is
      New_Sample : Point_2D;
   begin
      Debug_Message ("*** Warning: Trying to get next 2D sample from a Root Sampler");
      return New_Sample;
   end Get_Next_Sample;

   function Get_Next_Sample (Smp : in out Sampler) return Point_3D is
      New_Sample : Point_3D;
   begin
      Debug_Message ("*** Warning: Trying to get next 3D sample from a Root Sampler");
      return New_Sample;
   end Get_Next_Sample;

   function Get_Current_Sample (Smp : in out Sampler) return Point_2D is
      New_Sample : Point_2D;
   begin
      Debug_Message ("*** Warning: Trying to get current 2D sample from a Root Sampler");
      return New_Sample;
   end Get_Current_Sample;

   function Get_Current_Sample (Smp : in out Sampler) return Point_3D is
      New_Sample : Point_3D;
   begin
      Debug_Message ("*** Warning: Trying to get current 3D sample from a Root Sampler");
      return New_Sample;
   end Get_Current_Sample;

end Samplers;
