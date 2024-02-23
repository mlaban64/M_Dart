with Spectra;               use Spectra;
with Objects;               use Objects;
with Lights;                use Lights;
with Cameras;               use Cameras;
with Core_Types;            use Core_Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--<summary>The Scenes package contains all Abstract Data Types that make up a renderable scene</summary>
--<description>The Scenes package contains all Abstract Data Types that make up a renderable scene.
--It groups the objects, lights, camera and frame buffers to render the image</description>
--<group>Rendering</group>
package Scenes is

   -------------
   --  ADT Scene
   -------------

   type Scene is private;
   --<summary>ADT of a Scene, which carries all information to render a picture. Only used to implement The_World for now</summary>

   function Get_Camera return Camera_Ptr;

   function Get_Object_List return Object_List;

   function Get_CSG_Object_List return Object_List;

   function Get_CSG_Tree return Object_Ptr;

   function Get_Light_List return Light_List;

   function Get_Background_Color return RGB_Spectrum;

   function Get_Alarm_Color return RGB_Spectrum;

   function Get_Ambient_Light return RGB_Spectrum;

   function Use_Ambient_Light return Boolean;

   function Get_FileName return Unbounded_String;

   function Get_Name return Unbounded_String;

   procedure Set_Camera (Cam_Ptr : Camera_Ptr);
   --<summary>Sets the camera of The_World/summary>
   --<description>Sets the camera of The_World</description>
   --<parameter name="Cam_Ptr">Pointer to the camera</parameter>
   --<exception>None at this moment</exception>

   procedure Set_Object_List (Obj_List : in Object_List);
   --<summary>Sets the object list of The_World/summary>
   --<description>Sets the object list of The_World</description>
   --<parameter name="Obj_List">An object list</parameter>
   --<exception>None at this moment</exception>

   procedure Set_CSG_Object_List (Obj_List : in Object_List);
   --<summary>Sets the CSG object list of The_World/summary>
   --<description>Sets the CSG object list of The_World</description>
   --<parameter name="Obj_List">An object list</parameter>
   --<exception>None at this moment</exception>

   procedure Set_CSG_Tree (CSG_Tree : in Object_Ptr);
   --<summary>Sets the CSG Tree list of The_World/summary>
   --<description>Sets the CSG Tree list of The_World</description>
   --<parameter name="CSG_Tree">A CSG Tree</parameter>
   --<exception>None at this moment</exception>

   procedure Set_Light_List (Lt_List : in Light_List);
   --<summary>Sets the light list of The_World</summary>
   --<description>Sets the light list of The_World</description>
   --<parameter name="Lt_List">A light list</parameter>
   --<exception>None at this moment</exception>

   procedure Set_Background_Color (Bg_Col : RGB_Spectrum);
   --<summary>Sets the background color of The_World</summary>
   --<description>Sets the background of aThe_World</description>
   --<parameter name="Bg_Col">An RGB spectrum</parameter>
   --<exception>None at this moment</exception>

   procedure Set_Ambient_Light (Amb_Spec : RGB_Spectrum);
   --<summary>Sets the ambient light spectrum of The_World</summary>
   --<description>Sets the ambient light spectrum of The_World</description>
   --<parameter name="Amb_Spec">An RGB spectrum</parameter>
   --<exception>None at this moment</exception>

   procedure Use_Ambient_Light (Flag : Boolean);
   --<summary>Sets the use of ambient light of The_World</summary>
   --<description>Sets the use of ambient light of The_World</description>
   --<parameter name="Flag">Boolean to switch it off or on</parameter>
   --<exception>None at this moment</exception>

   procedure Set_FileName (FileName : in String);
   --<summary>Sets the FileName of The_World</summary>
   --<description>Sets the FileName of The_World</description>
   --<parameter name="FileName">A file path</parameter>
   --<exception>None at this moment</exception>

   procedure Set_Name (Name : in String);
   --<summary>Sets the Name of The_World</summary>
   --<description>Sets the Name of aThe_World</description>
   --<parameter name="Name">A name</parameter>
   --<exception>None at this moment</exception>

   procedure Render_Scene;
   --<summary>Renders The_World with a certain camera</summary>
   --<description>Renders The_World</description>
   --<exception>None at this moment</exception>

   function Get_Recursion_Level return Natural;
   --<summary>Returns the current Recursion_Level value for The_World</summary>
   --<description>Returns the current Recursion_Level value for The_World</description>
   --<exception>None at this moment</exception>

   procedure Increase_Recursion_Level;
   --<summary>Increases current Recursion_Level for The_World with 1</summary>
   --<description>Increases current Recursion_Level for The_World with 1</description>
   --<exception>None at this moment</exception>

   procedure Decrease_Recursion_Level;
   --<summary>Increases current Recursion_Level for The_World with 1</summary>
   --<description>Increases current Recursion_Level for The_World with 1</description>
   --<exception>None at this moment</exception>

   function Get_Max_Recursion_Level return Natural;
   --<summary>Returns the Max_Recursion_Level value for The_World</summary>
   --<description>Returns the Max_Recursion_Level value for The_World</description>
   --<exception>None at this moment</exception>

   procedure Set_Max_Recursion_Level (Level : in Natural);
   --<summary>Sets the Max_Recursion_Level to render The_World with/summary>
   --<description>Sets the Max_Recursion_Level to render The_World with</description>
   --<parameter name="Level">Level to set Max_Recustion_Level to</parameter>
   --<exception>None at this moment</exception>

   function Get_Recursion_Level_Reached return Natural;
   --<summary>Returns the Recursion_Level_Reached value for The_World</summary>
   --<description>Returns the Recursion_Level_Reached value for The_World</description>
   --<exception>None at this moment</exception>

   function Get_Minimum_Weight return Small_Float;
   --<summary>Returns the Minimum_Weight value for The_World</summary>
   --<description>Returns the Minimum_Weight value for The_World</description>
   --<exception>None at this moment</exception>

   procedure Set_Minimum_Weight (Min_Weight : in Small_Float);
   --<summary>Sets the Minimum_Weight to render The_World with/summary>
   --<description>Sets the Minimum_Weight to render The_World with</description>
   --<parameter name="Min_Weight">Min_Weight to set Minimum_Weight to</parameter>
   --<exception>None at this moment</exception>

private

   type Scene is record
      Obj_List : Object_List;
      --  The list of primary objects in the scene that are not part of the CSG objects
      CSG_Obj_List : Object_List;
      --  The list of primitive objects in the scene that are part of the CSG Tree
      CSG_Tree : Object_Ptr;
      --  The CSG Tree that builds up the scene
      Lt_List : Light_List;
      --  The list of lights in the scene
      Cam_Ptr : Camera_Ptr;
      --  The camera to take the picture with
      Ambient_Light : RGB_Spectrum := DARK_GREY_RGB_Spec;
      --  Ambient light source for the scene
      Use_Ambient_Light : Boolean := True;
      --  Flag to control shading with ambient light
      Background_Color : RGB_Spectrum := BLUE_RGB_Spec;
      --  Background color of the scene
      Alarm_Color : RGB_Spectrum := RED_RGB_Spec;
      --  Alarm color of the scene, to be used to flag something is rendered wrongly
      FileName : Unbounded_String := To_Unbounded_String ("/home/mlaban/scene.ppm");
      --  Path to store the rendered image in
      Name : Unbounded_String := To_Unbounded_String ("The World");
      --  Scene Name
      Recursion_Level : Natural := 0;
      --  Current Recursion Level
      Recursion_Level_Reached : Natural := 0;
      --  Recursion Level Reached
      Max_Recursion_Level : Natural := 30;
      --  Maximum Recursion Depth;
      Minimum_Weight : Small_Float := 0.000001;
   end record;

   -----
   --  GLOBAL VARIABLES
   ---
   The_World : Scene;

end Scenes;
