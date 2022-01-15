with System;
with BB; use BB;
with BB.Ideal; use BB.Ideal;
with BB.GUI; use BB.GUI;
with BB_PD_Control; use BB_PD_Control;
with Ada.Real_Time; use Ada.Real_Time;

procedure Seven_Targets is
   Targets : constant array (1 .. 8) of Position :=
   ( 1 | 3 | 5 | 7 => Max_Position - 10.0,
     2 | 4 | 6 | 8 => Min_Position + 10.0);
   
   task Controller
     with Priority => System.Priority'Last - 5
   is
      entry Start;
      entry Finish;
   end Controller;
   
   task body Controller is
      Period : constant Time_Span := Microseconds(62500);
      Next : Time;
      Ang : Angle;
   begin
      accept Start;
      Next := Clock;
      
      loop
         select
            accept Finish;
            exit;
         or
            delay until Next;
            
            Calculate_Action(Ball_Position, Ang);
            Set_Beam_Angle(Ang);
            
            Next := Next + Period;
         end select;
      end loop;
   end Controller;
begin
   Move_BB_To(Earth);
   Configure_PD(0.08, 0.8);
   Controller.Start;
   
   for I in Targets'Range loop
      Set_Target_Position(Targets(I));
      GUI_Setpoint(Targets(I));
      delay 3.0;
   end loop;
   
   Controller.Finish;
end Seven_Targets;
