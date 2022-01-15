with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

package ADC_Handler is

   procedure EOC_Handler;

   --  Suspension object for client to wait for end of conversion. The object
   --  is set True from EOC_Handler.
   End_Of_Conversion : Suspension_Object;

end ADC_Handler;
