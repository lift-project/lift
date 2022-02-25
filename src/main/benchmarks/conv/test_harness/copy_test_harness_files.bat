@echo off
SET testHarnessDir=%1
SET destinationDir=%2
SET testHarnessFName=%3

xcopy /q /y %testHarnessDir%\deinit_experiment_env.sh %destinationDir%\ >nul 2>&1
xcopy /q /y %testHarnessDir%\golden_conv_impl.cpp %destinationDir%\ >nul 2>&1
xcopy /q /y %testHarnessDir%\init_experiment_env.sh %destinationDir%\ >nul 2>&1
xcopy /q /y %testHarnessDir%\%testHarnessFName% %destinationDir%\ >nul 2>&1
echo Copied test benchmark files to %destinationDir%