################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
CPP_SRCS += \
../BackEase.cpp \
../BounceEase.cpp \
../CircularEase.cpp \
../CubicEase.cpp \
../EasingBase.cpp \
../ElasticEase.cpp \
../ExponentialEase.cpp \
../LinearEase.cpp \
../QuadraticEase.cpp \
../QuarticEase.cpp \
../QuinticEase.cpp \
../SineEase.cpp 

OBJS += \
./BackEase.o \
./BounceEase.o \
./CircularEase.o \
./CubicEase.o \
./EasingBase.o \
./ElasticEase.o \
./ExponentialEase.o \
./LinearEase.o \
./QuadraticEase.o \
./QuarticEase.o \
./QuinticEase.o \
./SineEase.o 

CPP_DEPS += \
./BackEase.d \
./BounceEase.d \
./CircularEase.d \
./CubicEase.d \
./EasingBase.d \
./ElasticEase.d \
./ExponentialEase.d \
./LinearEase.d \
./QuadraticEase.d \
./QuarticEase.d \
./QuinticEase.d \
./SineEase.d 


# Each subdirectory must supply rules for building sources it contributes
%.o: ../%.cpp
	@echo 'Building file: $<'
	@echo 'Invoking: AVR C++ Compiler'
	avr-g++ -Wall -Werror -Os -fpack-struct -fshort-enums -funsigned-char -funsigned-bitfields -fno-exceptions -mmcu=atmega16 -DF_CPU=1000000UL -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -c -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


