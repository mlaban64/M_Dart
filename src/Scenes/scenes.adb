with M_GraphiX;              use M_GraphiX;
with Utilities;              use Utilities;
with Tracers;                use Tracers;
with Tone_Maps;              use Tone_Maps;
with Linear_Math;            use Linear_Math;
with Ada.Text_IO;            use Ada.Text_IO;
with Small_Float_Functions;  use Small_Float_Functions;
with Normal_Float_Functions; use Normal_Float_Functions;
with Large_Float_Functions;  use Large_Float_Functions;

package body Scenes is

   -------------
   --  ADT Scene
   -------------

   function Get_Camera return Camera_Ptr is (The_World.Cam_Ptr);

   function Get_Object_List return Object_List is (The_World.Obj_List);

   function Get_CSG_Object_List return Object_List is (The_World.CSG_Obj_List);

   function Get_CSG_Tree return Object_Ptr is (The_World.CSG_Tree);

   function Get_Light_List return Light_List is (The_World.Lt_List);

   function Get_Background_Color return RGB_Spectrum is (The_World.Background_Color);

   function Get_Alarm_Color return RGB_Spectrum is (The_World.Alarm_Color);

   function Get_Ambient_Light return RGB_Spectrum is (The_World.Ambient_Light);

   function Use_Ambient_Light return Boolean is (The_World.Use_Ambient_Light);

   function Get_FileName return Unbounded_String is (The_World.FileName);

   function Get_Name return Unbounded_String is (The_World.Name);

   procedure Set_Camera (Cam_Ptr : in Camera_Ptr) is
   begin
      The_World.Cam_Ptr := Cam_Ptr;
   end Set_Camera;

   procedure Set_Object_List (Obj_List : in Object_List) is
   begin
      The_World.Obj_List := Obj_List;
   end Set_Object_List;

   procedure Set_CSG_Object_List (Obj_List : in Object_List) is
   begin
      The_World.CSG_Obj_List := Obj_List;
   end Set_CSG_Object_List;

   procedure Set_CSG_Tree (CSG_Tree : in Object_Ptr) is
   begin
      The_World.CSG_Tree := CSG_Tree;
   end Set_CSG_Tree;

   procedure Set_Light_List (Lt_List : in Light_List) is
   begin
      The_World.Lt_List := Lt_List;
   end Set_Light_List;

   procedure Set_Background_Color (Bg_Col : RGB_Spectrum) is
   begin
      The_World.Background_Color := Bg_Col;
   end Set_Background_Color;

   procedure Set_Ambient_Light (Amb_Spec : RGB_Spectrum) is
   begin
      The_World.Ambient_Light := Amb_Spec;
   end Set_Ambient_Light;

   procedure Use_Ambient_Light (Flag : Boolean) is
   begin
      The_World.Use_Ambient_Light := Flag;
   end Use_Ambient_Light;

   procedure Set_FileName (FileName : in String) is
   begin
      The_World.FileName := To_Unbounded_String (FileName);
   end Set_FileName;

   procedure Set_Name (Name : in String) is
   begin
      The_World.Name := To_Unbounded_String (Name);
   end Set_Name;

   function Get_Recursion_Level return Natural is (The_World.Recursion_Level);

   function Get_Recursion_Level_Reached return Natural is (The_World.Recursion_Level_Reached);

   function Get_Max_Recursion_Level return Natural is (The_World.Max_Recursion_Level);

   procedure Set_Max_Recursion_Level (Level : in Natural) is
   begin
      if Level < 1 then
         Debug_Message ("*** ERROR: Max_Recursion_Level is set below 1", 1);
      end if;
      Debug_Message ("*** Information: Max_Recursion_Level is set to " & Level'Image, 1);
      The_World.Max_Recursion_Level := Level;
   end Set_Max_Recursion_Level;

   procedure Increase_Recursion_Level is
   begin
      The_World.Recursion_Level := The_World.Recursion_Level + 1;
      if The_World.Recursion_Level > The_World.Recursion_Level_Reached then
         The_World.Recursion_Level_Reached := The_World.Recursion_Level;
      end if;
   end Increase_Recursion_Level;

   procedure Decrease_Recursion_Level is
   begin
      The_World.Recursion_Level := The_World.Recursion_Level - 1;
   end Decrease_Recursion_Level;

   function Get_Minimum_Weight return Small_Float is (The_World.Minimum_Weight);

   procedure Set_Minimum_Weight (Min_Weight : Small_Float) is
   begin
      The_World.Minimum_Weight := Min_Weight;
      if Min_Weight > (1.0 / 512.0) then
         Debug_Message ("*** Warning: Minimum Weight is set to " & Min_Weight'Image);
      else
         Debug_Message ("*** Information: Minimum Weight is set to " & Min_Weight'Image);
      end if;
   end Set_Minimum_Weight;

   procedure Render_Scene is
      X_Res, Y_Res : Natural;
      Prim_Ray     : Ray;
      Ext_Rad      : RGB_Spectrum;
      Cam_Ptr      : Camera_Ptr;
   begin
      --  Initialize
      Cam_Ptr := The_World.Cam_Ptr;
      X_Res   := Cam_Ptr.Get_X_Res;
      Y_Res   := Cam_Ptr.Get_Y_Res;

      --  Open the screen & image
      Open_Image_Window (X_Res, Y_Res, True, Get_Name);
      Create_Tone_Map (X_Res, Y_Res);
      Opening_Message;

      --  Print some scene rendering information
      Put (The_World.Ambient_Light, "Ambient Light Spectrum: ");
      Put ("Ambient Light Usage: ");
      Put (The_World.Use_Ambient_Light'Image);
      New_Line (2);
      --  Construct & trace the init ray
      Prim_Ray := Construct_Ray (The_World.Cam_Ptr.Get_Position, X_AXIS_3D);
      Trace_Init_Ray (Prim_Ray);

      --  For each pixel, sample it and color it
      for Y in 0 .. (Y_Res - 1) loop
         for X in 0 .. (X_Res - 1) loop

            --  The ray is defined in world coordinates, but we need to flip Y, as (0,0) is upper-left on screen Prim_Ray :=
            Debug_Message ("=== START SAMPLING NEW PIXEL AT (" & X'Image & "," & Y'Image & ")===", 5);
            Ext_Rad := Cam_Ptr.Sample_Pixel (X, Y_Res - 1 - Y);
            Debug_Message ("=== END SAMPLING NEW PIXEL AT (" & X'Image & "," & Y'Image & ")===", 5);
            --  Regenerate samples for the next pixel; must be done smarter !!!
            Cam_Ptr.all.Get_Pixel_Sampler.Initialize;
            --  Convert the radiance into a screen color
            Set_Pixel_With_Buffer_Radiance (X, Y, Float (Get_R (Ext_Rad)), Float (Get_G (Ext_Rad)), Float (Get_B (Ext_Rad)));
            --  Set the pixel in the tone map
            Set_Pixel (X, Y, Ext_Rad);

         end loop;
         --  Check if a 'q' key was clicked during the rendering. If so, exit
         if GfxLib_CheckEvent_Waiting > 0 then
            if GfxLib_WaitForEvent = 'q' then
               exit;
            end if;
         end if;

      end loop;

      --  Tone Mapping, close screen and save image
      Closing_Message;

      Put_Line ("Press any key to start the Tone Mapping process");
      Put (GfxLib_WaitForEvent);

      Map_Tones_To_Image_Linear;
      -- Map_Tones_To_Image_Reinhard;
      -- Map_Tones_To_Image_Reinhard_Extended;
      Put_Line ("Press any key to save the image and quit");
      Put (GfxLib_WaitForEvent);
      Save_Image (Get_FileName);
      Put_Line ("Image saved to " & To_String (Get_FileName));
      GfxLib_CloseWindow;

   end Render_Scene;

end Scenes;
