with BB; use BB;
with BB_PD_Control; use BB_PD_Control;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Test_BB_PD_Control is
   Pos : array (1 .. 10) of Position :=
     (0.000, 0.271, 0.631, 1.043, 1.488,
      1.934, 2.381, 2.815, 3.242, 3.642);
   Sgn : Float := 1.0;
   Offset : Integer := 0;
   Ang : Angle;

begin

   Configure_PD (Kp     => 2.0,
                 Kd     => 35.0,
                 Target => 10.0);

   Put_Line ("Target = 10.0");
   for I in Pos'Range loop
      Calculate_Action (Sgn * Pos (I), Ang);
      Put ("Angle" & Integer'Image(I + Offset) & " = ");
      Put (Ang, Exp => 0);
      New_Line;
   end loop;

   Set_Target_Position (-14.0);
   Sgn := - Sgn;
   Offset := 10;
   Put_Line ("Target = -14.0");
   for I in Pos'Range loop
      Calculate_Action (Sgn * Pos (I), Ang);
      Put ("Angle" & Integer'Image(I + Offset) & " = ");
      Put (Ang, Exp => 0);
      New_Line;
   end loop;

end Test_BB_PD_Control;
