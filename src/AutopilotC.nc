#include "IMU.h"
#include "Vector3.h"

#define   IMU_PERIOD 4 // Poll   the IMU    every  IMU_PERIOD                 ms.
#define MOTOR_PERIOD 5 // Adjust the motors every (IMU_PERIOD * MOTOR_PERIOD) ms.

// Provides a program for the mote controlling the helicopter.
module AutopilotC {
  uses {
    interface Boot;
    interface Receive;
    interface Timer <TMilli>;
    interface IMU;
    interface StdControl as IMUControl;
    interface Motors;
    interface Init as MotorsInit;
    interface PID <Vector3> as LinearPID;
    interface PID <float>   as YawPID   ;
    interface SplitControl as AMControl;
    interface AMSend;
    interface Packet;
    interface DeadReckoning;
    interface StdControl as MuxControl;
    interface Init as MuxInit;
    interface GeneralIO as MuxSelect;
    interface Leds;
    interface BusyWait <TMicro, uint16_t>;
    interface Spi2Byte;
  }
}

implementation {

  bool autopilotActive;

  Vector3 targetPosition;
  float targetYaw;

  event void Boot.booted ()
  {

    call MuxInit.init ();

    call MotorsInit.init ();

    autopilotActive = FALSE;
    targetPosition = zeroV3;
    targetYaw = 0;

    // Initialize the PIDs with weights of (1, 1, 1) and initial previous error and integral of zero.
    call LinearPID.initialize (1, 1, 1, zeroV3, zeroV3);
    call YawPID.initialize    (1, 1, 1, 0     , 0     );
    call DeadReckoning.initialize (zeroV3, zeroV3);
    call AMControl.start ();

    call IMUControl.start ();

    call Timer.startPeriodic (IMU_PERIOD);

  }

  // This callback inspects the contents of the message.  If it is 'A', then the autopilot is activated.  If it is 'D', then the autopilot is deactivated.
  event message_t *Receive.receive (message_t *bufPtr, void *payload, uint8_t len)
  {
    char directive = *(char*)payload;
    dbg ("Autopilot", "directive: %c; length: %d\n", directive, len);
    switch (directive) {
    case 'A':
      if (! autopilotActive) {
        call Timer.startPeriodic (IMU_PERIOD);
	    call MuxControl.start ();
        autopilotActive = TRUE;
        dbg ("Autopilot", "Autopilot activated\n");
      }
      break;
    case 'D':
      if (autopilotActive) {
        call MuxControl.stop ();
        call Timer.stop ();
        autopilotActive = FALSE;
        dbg ("Autopilot", "Autopilot deactivated\n");
      }
      break;
    default:
      dbg ("Autopilot", "Junk directive: &c\n", directive);
      }
    return bufPtr;
  }

  event void AMSend.sendDone (message_t *bufPtr, error_t error) { }

  event void AMControl.startDone (error_t err) {
    if (err == SUCCESS) {
    }
    else {
      call AMControl.start ();
    }
  }

  event void AMControl.stopDone (error_t err) { }

//Accelerometer = mg/LSB
//Gyro = degrees/sec/LSB

#define pi 3.141592653589793

#define CHECKDATA(prev, reg) do { data = call IMU.readRegister (reg); LAandAV.prev = ((float) ((int16_t) (data << 2))) / 4; } while (0)
  void updateData ()
  {

    static uint8_t tick = 0;
    static DoubleVector3 LAandAV;
    uint16_t data;
    Vector3 position, orientation, absoluteLinearCorrection, linearCorrection;
    DoubleVector3 positionAndOrientation;
    float yawCorrection;

    int16_t accl;

    tick++;

    call IMU.readRegister (YACCL_OUT);
    accl = (call IMU.readRegister (YACCL_OUT) << 2) / 4; // (float) ((int16_t) (call IMU.readRegister (YACCL_OUT) << 2));

    if      (accl >=  200)
      call Leds.set (1); // 001
    else if (accl <= -200)
      call Leds.set (4); // 100
    else
      call Leds.set (2); // 010

    call IMU.readRegister (XACCL_OUT);

    CHECKDATA (a.x, YACCL_OUT);
    CHECKDATA (a.y, ZACCL_OUT);
    CHECKDATA (a.z, XGYRO_OUT);
    CHECKDATA (b.pitch, YGYRO_OUT);
    CHECKDATA (b.roll , ZGYRO_OUT);
    CHECKDATA (b.yaw  , ZGYRO_OUT);

    //position is in terms of undefined units; orientation is in terms of radians.
    // Changed all time units to seconds.
    positionAndOrientation = call DeadReckoning.updateReckoning (((float)IMU_PERIOD)/1000, LAandAV.a, scaleV3 (GYRO_SCALE * pi / 180, LAandAV.b));
    position = positionAndOrientation.a; orientation = positionAndOrientation.b;
    yawCorrection            = call    YawPID.updateError (((float)IMU_PERIOD)/1000, targetYaw - orientation.yaw);
    absoluteLinearCorrection = call LinearPID.updateError (((float)IMU_PERIOD)/1000, addV3 (targetPosition, scaleV3 (-1, position)));
    linearCorrection = absoluteToRelativeV3 (orientation, absoluteLinearCorrection);

    if (tick % MOTOR_PERIOD == 0) {
      // T + B = z
      // T - B = yaw
      // -----------
      // T = z + yaw
      // B = z - yaw
      call Motors.setTopRotorPower    (linearCorrection.z + yawCorrection);
      call Motors.setBottomRotorPower (linearCorrection.z - yawCorrection);
      call Motors.setPitchPower       (linearCorrection.y                );
      call Motors.setRollPower        (linearCorrection.x                );
    }

  }

  event void Timer.fired () {
    updateData ();
  }

}
