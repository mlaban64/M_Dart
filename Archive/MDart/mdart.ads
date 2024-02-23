--  Package MDart
--
--  This package contains some supporting functions for the main procedure

package MDart is

   M_Dart_Version : constant String := "01.00.00.00";
   --  <summary>M_Dart_Version contains the version number of M-Dart.
   --  it should be replaced by some VCS parameter in the future</summary>

   procedure Opening_Message;
   --  <summary>Opening_Message is a procedure to print a start-up message</summary>

   procedure Closing_Message;
   --  <summary>Closing_Message is a procedure to print a closing message</summary>

end MDart;
