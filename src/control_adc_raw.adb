with System;
with BB;                           use BB;
with BB.ADC;                       use BB.ADC;
with BB.GUI;                       use BB.GUI;
with BB_PD_Control;                use BB_PD_Control;
with Ada.Real_Time;                use Ada.Real_Time;
with CSV_Logs;                     use CSV_Logs;
with ADC_Handler;                  use ADC_Handler;
with ADC_Utils; use ADC_Utils;
with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

procedure Control_ADC_Raw is
   task Controller with
      Priority => System.Priority'Last - 5
   is
      entry Start;
   end Controller;

   task body Controller is
      Period : constant Time_Span := Microseconds (62500);
      Next   : Time;

      Conversion : ADC_Register;  --  To store ADU_Count
      Pos_mm     : Position;      --  Position corresponding to Conversion
      Ang        : Angle;
   begin
      accept Start;
      Next := Clock;

      for I in 1 .. 100 loop
         delay until Next;

         -- Trigger conversion
         Set_False (End_Of_Conversion);
         Write_CR (Get_CR (Trigger => True, Interrupt_Enable => True));

         -- Wait for EOC Suspension Object
         Suspend_Until_True (End_Of_Conversion);

         -- Obtain first 12 MSBs
         Conversion := Read_DR and (2**12 - 1);

         -- Convert value from ADC to mm
         Pos_mm := ADC_To_Position (Conversion);

         -- Write the conversion result
         Calculate_Action (Pos_mm, Ang);
         Set_Beam_Angle (Ang);
         Log_Data ((Pos_mm, Ang));

         Next := Next + Period;
      end loop;
   end Controller;
begin
   Open_Log_Session (File_Name => "control_raw.csv");
   Log_Text ("Pos_mm, Ang");

   -- Register interrupt handler
   Attach_ADC_Handler (EOC_Handler'Access);
   Move_BB_To (Earth);
   Configure_PD (Kp => 2.0, Kd => 35.0, Target => 0.0);
   GUI_Setpoint (0.0);

   Controller.Start;
end Control_ADC_Raw;
