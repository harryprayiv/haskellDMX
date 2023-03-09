void kpdToCommand(char key) {
  switch (key) {
    //___________________________________________________________________________________________________
    case '@':                       //  fall through switch for the '@' key with function trigger
    case 'T':                       //  fall through switch for the 'T' (through) key with function trigger
    case '&':                       //  fall through switch for the '&' key with function trigger
    case '-':                       //  fall through switch for the '-' key with function trigger
    case 'E':                       //  fall through switch for the 'E' key with function trigger
    case 'S':                       //  mapping for 'S' key with function trigger
      keypadLogic(false, key);
      break;
    case '0':                       //  fall through switch for the '0' key with function trigger
    case '1':                       //  fall through switch for the '1' key with function trigger
    case '2':                       //  fall through switch for the '2' key with function trigger
    case '3':                       //  fall through switch for the '3' key with function trigger
    case '4':                       //  fall through switch for the '4' key with function trigger
    case '5':                       //  fall through switch for the '5' key with function trigger
    case '6':                       //  fall through switch for the '6' key with function trigger
    case '7':                       //  fall through switch for the '7' key with function trigger
    case '8':                       //  fall through switch for the '8' key with function trigger
    case '9':                       //  mapping for '9' key with function trigger
      keypadLogic(true, key);
      break;
  }
}

void keypadLogic(bool isAnInteger, char kpdInput) {
  switch (kpdState) {
    //MODE_SELECT______________________________________
    case MODE_SELECT:
      if ((isAnInteger == false) && (pgmModeSelectionInt > 0)) {
        if (kpdInput == 'E') {
          if (pgmModeSelectionInt == 1) {
            smpleDisplay("Fader Mode", true, true);
            controlMode = FADER_MODE;
            kpdState = NO_CMD;
            modeChosen = true;
            break;
          } if (pgmModeSelectionInt == 2) {
            smpleDisplay("Keypad Mode", true, true);
            controlMode = KPD_MODE;
            kpdState = NO_CMD;
            modeChosen = true;
            break;
          } if (pgmModeSelectionInt == 3) {
            smpleDisplay("Keypad Fader Mode", true, true);
            controlMode = KPDFADER_MODE;
            kpdState = NO_CMD;
            modeChosen = true;
            break;
          }
          kpdState = MODE_SELECT; // any non-integer than "Enter" won't work here
          break;
        }
        kpdState = MODE_SELECT;
        break;
      } else {
        if (kpdInput == '1') {      //don't count a zero as the first integer in the array
          pgmModeSelectionInt = 1;
          smpleDisplay(pgmModeSelectionInt, true, true);
          kpdState = MODE_SELECT;
          break;
        } if (kpdInput == '2') {     //2 equals
          pgmModeSelectionInt = 2;
          smpleDisplay(pgmModeSelectionInt, true, true);
          kpdState = MODE_SELECT;
          break;
        } if (kpdInput == '3') {     //don't count a zero as the first integer in the array
          pgmModeSelectionInt = 3;
          smpleDisplay(pgmModeSelectionInt, true, true);
          kpdState = MODE_SELECT;
          break;
        } else {
          smpleDisplay("Number not allowed", true, true);
          kpdState = MODE_SELECT;
          break;
        }
      }
    //NO_CMD______________________________________
    case NO_CMD:
      if (isAnInteger == false) {
        kpdState = NO_CMD;
        break;
      } else {
        if (kpdInput == '0') {      //don't count a zero as the first integer in the array
          kpdState = NO_CMD;
          break;
        }
        chOneKpdChar[intCount] = kpdInput;
        smpleDisplay(chOneKpdChar, true, true);
        intCount++;
        kpdState = DMXCH_ONE;
        break;
      }
    //___DMXCH_ONE____________________________________
    case DMXCH_ONE:                                                     // First Channel Assignment command
      if (isAnInteger == false) {                                     // is this an integer?
        /*___________AT__________________________*/
        if ((kpdInput == '@') && (intCount > 0)) { // if it is '@' and there are more than 0 integers
          smpleDisplay(" @ ", true, true);
          channelOneInt = atoi (chOneKpdChar);  //parse array into an int
          if (channelOneInt > 512) {                    //greater than 512?? (one universe)
            channelOneInt = 512;                      // max out channel number to 512
          }
          selectionType = SINGLECHANNEL;          // classify the command as soon as it is known
          kpdState = DMX_INTENSITY;                  // move to the stage where we assign intensity
          intCount = 0;      //zero the int Count
          break;
        }
        /*___________THROUGH__________________________*/
        if ((kpdInput == 'T') && (intCount > 0)) { // if it is 'T' and there are more than 0 integers
          smpleDisplay(" thru ", true, true);
          channelOneInt = atoi (chOneKpdChar); //parse array into an int
          if (channelOneInt > 512) {            //greater than 512?? (one universe)
            channelOneInt = 512;              // max out channel number to 512
          }
          selectionType = THROUGH;   // enum & select proper bool index
          kpdState = DMXCH_TWO;                  // move to the stage where you assign intensity
          intCount = 0;      //zero the int Count
          break;
        }
        /*___________AND__________________________*/
        if ((kpdInput == '&') && (intCount > 0)) { // if it is '&' and there are more than 0 integers
          smpleDisplay(" and ", true, true);
          channelOneInt = atoi (chOneKpdChar);    //parse array into an int
          if (channelOneInt > 512) {
            channelOneInt = 512;
          }
          selectionType = AND;   // enum & select proper bool index
          kpdState = DMXCH_TWO;                  // move to the stage where you assign intensity
          intCount = 0;      //parse and zero the int Count
          break;
        }
        break;
        /*___________3 INTEGERS__________________________*/
      } else if (intCount == 2) {                          //more than 2 integer places
        chOneKpdChar[intCount - 2] = chOneKpdChar[intCount - 1]; //shifting values to next array position
        chOneKpdChar[intCount - 1] = chOneKpdChar[intCount]; //shifting values to next array position
        chOneKpdChar[intCount] = kpdInput;   // adding the char to the array
        smpleDisplay(chOneKpdChar, true, true);
        kpdState = DMXCH_ONE;                   // keep wrapping digits in this controlModeuntil modifier
        intCount = 2;
        break;
        /*___________< 3 INTEGERS________________________*/
      } else {                                                 // if we aren't overflowing, do this
        chOneKpdChar[intCount] = kpdInput;   // adding the char to the array
        smpleDisplay(chOneKpdChar, true, true);
        kpdState = DMXCH_ONE;                   // stay in this controlModeuntil modifier is pressed
        intCount++;
        break;                                          // leave the switch
      }
    //___DMXCH_TWO____________________________________
    case DMXCH_TWO:                  // Second Channel Assignment command
      if (isAnInteger == false) {                                     // is this an integer?
        /*___________AT__________________________*/
        if ((kpdInput == '@') && (intCount > 0)) { // if input = '@' and > than 0 integers
          /*___________AND__________________________*/
          if (selectionType == AND) {            // if input = 'AND'
            smpleDisplay(" at ", true, true);
            channelTwoInt = atoi (chTwoKpdChar);  //parse array into an int
            if (channelTwoInt > 512) {            // if input > 512
              channelTwoInt = 512;              // make it 512
            }
            kpdState = DMX_INTENSITY;                  // move to the stage where you assign intensity
            intCount = 0;      //parse and zero the int Count
            break;
            /*___________THROUGH__________________________*/
          } if (selectionType == THROUGH) {           // if input = 'THROUGH'
            smpleDisplay(" at ", true, true);
            channelTwoInt = atoi (chTwoKpdChar);      //parse array to an int
            if (channelTwoInt > 512) {          // prevent values from going over the max
              channelTwoInt = 512;
            }
            kpdState = DMX_INTENSITY;                  // move to the stage where you assign intensity
            intCount = 0;      //parse and zero the int Count
            break;
          }
        }
        break;
        /*___________< 3 INTEGERS________________________*/
      } else if (intCount == 2) {                          //more than 2 integer places
        chTwoKpdChar[intCount - 2] = chTwoKpdChar[intCount - 1]; chTwoKpdChar[intCount - 1] = chTwoKpdChar[intCount]; //shifting values to next array position
        chTwoKpdChar[intCount] = kpdInput;   // adding the char to the array
        smpleDisplay(chTwoKpdChar, true, true);
        kpdState = DMXCH_TWO;                   // keep wrapping digits in this controlModeuntil modifier
        intCount = 2;
        break;
        /*___________< 3 INTEGERS________________________*/
      } else {                                                 // if we aren't overflowing, do this
        chTwoKpdChar[intCount] = kpdInput;   // adding the char to the array
        smpleDisplay(chTwoKpdChar, true, true);
        kpdState = DMXCH_TWO;                   // stay in this controlModeuntil modifier is pressed
        intCount++;
        break;                                          // leave the switch
      }

    //___DMX_INTENSITY____________________________________
    case DMX_INTENSITY:                  // Intensity Assignment Part of the Function
      if (isAnInteger == false) {
        if ((kpdInput == 'E') && (intCount > 0)) {
          if (controlMode == KPD_MODE) {      // if it is in KPD_MODE control mode
            if (selectionType == SINGLECHANNEL) {
              intCount = 0;
              kpdIntensityFloat = atof (intensityString);
              dmxDisplay(channelOneInt, SINGLECHANNEL, channelTwoInt, intensityString, true, true);
              kpdSubIntensity(channelOneInt, SINGLECHANNEL, 0, kpdIntensityFloat);
              kpdState = NO_CMD;
              selectionType = NONE; channelOneInt = 0; channelTwoInt = 0; intensityString[0] = '0';
              break;
            } if (selectionType == AND) {
              intCount = 0;
              kpdIntensityFloat = atof (intensityString);
              dmxDisplay(channelOneInt, AND, channelTwoInt, intensityString, true, true);
              kpdSubIntensity(channelOneInt, AND, channelTwoInt, kpdIntensityFloat);
              kpdState = NO_CMD;
              break;
            } if (selectionType == THROUGH) {
              intCount = 0;
              kpdIntensityFloat = atof (intensityString);
              dmxDisplay(channelOneInt, THROUGH, channelTwoInt, intensityString, true, true);
              kpdSubIntensity(channelOneInt, THROUGH, channelTwoInt, kpdIntensityFloat);
              kpdState = NO_CMD;
              break;
            }
          } if (controlMode == KPDFADER_MODE) {     // if it is in KPDFADER_MODE control mode
            if (selectionType == SINGLECHANNEL) {
              int i = atoi (intensityString);
              dmxDisplay(channelOneInt, SINGLECHANNEL, 0, (analogFaderMap[i - 1]), true, true);
              kpdfaderSubIntensity(channelOneInt, SINGLECHANNEL, 0, (analogFaderMap[i - 1]));
              kpdState = NO_CMD;
              intCount = 0;
              break;
            } if (selectionType == AND) {
              int i = atoi (intensityString);
              dmxDisplay(channelOneInt, AND, channelTwoInt, (analogFaderMap[i - 1]), true, true);
              kpdfaderSubIntensity(channelOneInt, AND, channelTwoInt, (analogFaderMap[i - 1]));
              kpdState = NO_CMD;                  // move to the stage where you assign intensity
              intCount = 0;
              break;
            } if (selectionType == THROUGH) {
              int i = atoi (intensityString);
              dmxDisplay(channelOneInt, THROUGH, channelTwoInt, (analogFaderMap[i - 1]), true, true);
              kpdfaderSubIntensity(channelOneInt, THROUGH, channelTwoInt, (analogFaderMap[i - 1]));
              kpdState = NO_CMD;                  // move to the stage where you assign intensity
              intCount = 0;
              break;
            }
          }
        } break;
        /*___________ONLY ALLOW 1 through 8 keys Representing Faders__________________________*/
      } else if ((controlMode == KPDFADER_MODE) && (kpdInput != '0') && (kpdInput != '9')) {
        intCount = 0;
        intensityString[intCount] = kpdInput;
        kpdState = DMX_INTENSITY;
        smpleDisplay(intensityString, true, true);
        intCount = 1;
        break;
      }
      /*___________9 INTEGERS__________________________*/
      else if ((controlMode == KPD_MODE) && (intCount > 8 )) {
        intWrap(intensityString, kpdInput, 9);
        kpdState = DMX_INTENSITY;
        smpleDisplay(intensityString, true, true);
        intCount = 9;
        break;
        /*___________>9 INTEGERS__________________________*/
      } else if ((controlMode == KPD_MODE) && (intCount < 9 )) {
        intensityString[intCount] = kpdInput;
        kpdState = DMX_INTENSITY;
        smpleDisplay(intensityString, true, true);
        intCount++;
        break;
      }
  }
}


// to call this function: faderSubIntensity(1, 27, THROUGH, 1) would get you a submaster of channels 1 through 27 on fader 1
// to call this function: faderSubIntensity(1, 27, AND, 1) would get you a submaster of channels 1 and 27 on fader 1
// to call this function: faderSubIntensity(1, 0, SINGLECHANNEL, 1) would get you a submaster of channel 1 on fader 1
void kpdfaderSubIntensity(int chOne, selectionMode selType, int chTwo, int sMfader) {
  switch (selType) {
    /*_______PARSING 'SINGLECHANNEL'__________*/
    case SINGLECHANNEL:
      scalerVal = (floatmap(analogRead(9), 1, 65536, 65025, 1) / 65025);   // Master Fader__________________
      if (scalerVal <= .01) {              // eliminate small values to avoid flickering
        scalerVal = 0;
      }
      dmxVal[chOne - 1] = round(floatmap(analogRead(sMfader), 1, 65536, 255, 1) * scalerVal);
      if (dmxVal[chOne - 1] < 2) {                  // eliminate small values to avoid flickering (I may eventually do smoothing instead)
        dmxVal[chOne - 1] = 0;
      }
      Dmx.setDmxChannel(chOne, dmxVal[chOne - 1]);
      break;
    /*_______PARSING 'AND'____________________*/
    case AND:
      scalerVal = (floatmap(analogRead(9), 1, 65536, 65025, 1) / 65025);   // Master Fader__________________
      if (scalerVal <= .01) {              // eliminate small values to avoid flickering
        scalerVal = 0;
      }
      dmxVal[chOne - 1] = round(floatmap(analogRead(sMfader), 1, 65536, 255, 1) * scalerVal);
      if (dmxVal[chOne - 1] < 2) {                // eliminate small values to avoid flickering (I may eventually do smoothing instead)
        dmxVal[chOne - 1] = 0;
      }
      Dmx.setDmxChannel(chOne, dmxVal[chOne - 1]);
      dmxVal[chTwo - 1] = round(floatmap(analogRead(sMfader), 1, 65536, 255, 1) * scalerVal);
      if (dmxVal[chTwo - 1] < 2) {                // eliminate small values to avoid flickering (I may eventually do smoothing instead)
        dmxVal[chTwo - 1] = 0;
      }
      Dmx.setDmxChannel(chTwo, dmxVal[chTwo - 1]);
      break;
    /*_______PARSING 'THROUGH'__________________*/
    case THROUGH:
      scalerVal = (floatmap(analogRead(9), 1, 65536, 65025, 1) / 65025);   // Master Fader__________________
      if (scalerVal <= .01) {              // eliminate small values to avoid flickering
        scalerVal = 0;
      }
      for (int i = (chOne - 1); i > (chTwo - chOne); ++i) {
        dmxVal[i] = round(floatmap(analogRead(sMfader), 1, 65536, 255, 1) * scalerVal);
        if (dmxVal[i] < 2) {                  // eliminate small values to avoid flickering (I may eventually do smoothing instead)
          dmxVal[i] = 0;
        }
        Dmx.setDmxChannel((i + 1), dmxVal[i]);
      }
      break;
    case NONE:
      break;
  }
}

// to call this function: kpdSubIntensity(1, SINGLECHANNEL, 0, 100.00) would get you channel 1 @ 100% of 255
// to call this function: kpdSubIntensity(1, AND, 37, 66.666666) would get you channel 1 AND 37 @ 66% of 255
// to call this function: kpdSubIntensity(1, THROUGH, 37, 75.000) would get you channel 1 THROUGH 37 @ 75% of 255
void kpdSubIntensity(int chOne, selectionMode selType, int chTwo, float intensity) {
  switch (selType) {
    /*_______PARSING 'SINGLECHANNEL'__________*/
    case SINGLECHANNEL:
      dmxVal[chOne - 1] = round(floatmap(intensity, 0, .99999999, 0, 255)); // map float percent intensity to integer of 0-255
      Dmx.setDmxChannel(chOne, dmxVal[chOne - 1]);
      break;
    /*_______PARSING 'AND'____________________*/
    case AND:
      dmxVal[chOne - 1] = round(floatmap(intensity, 0, .99999999, 0, 255)); // map float percent intensity to integer of 0-255
      Dmx.setDmxChannel(chOne, dmxVal[chOne - 1]);
      dmxVal[chTwo - 1] = round(floatmap(intensity, 0, .99999999, 0, 255)); // map float percent intensity to integer of 0-255
      Dmx.setDmxChannel(chTwo, dmxVal[chTwo - 1]);
      break;
    /*_______PARSING 'THROUGH'__________________*/
    case THROUGH:
      if (chOne == chTwo) {
        dmxVal[chOne - 1] = round(floatmap(intensity, 0, .99999999, 0, 255)); // map float percent intensity to integer of 0-255
        Dmx.setDmxChannel(chOne, dmxVal[chOne - 1]);
      }
      if (chOne < chTwo) {
        for (int i = (chOne - 1); i > (chTwo - chOne); ++i) {       //loop through all channels and assign this intensity
          dmxVal[i] = round(floatmap(intensity, 0, .99999999, 0, 255));
          Dmx.setDmxChannel((i + 1), dmxVal[i]);
        }
        break;
      } if (chOne > chTwo) {
        for (int i = (chTwo - 1); i > (chOne - chTwo); ++i) {       //loop through all channels and assign this intensity
          dmxVal[i] = round(floatmap(intensity, 0, .99999999, 0, 255));
          Dmx.setDmxChannel((i + 1), dmxVal[i]);
        }
        break;
      }
    case NONE:
      break;
  }
}
