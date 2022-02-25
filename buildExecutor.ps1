$DIR = $PSScriptRoot
$Env:OPENCL_LIB_DIR = "C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v10.2\\lib\\x64"
$Env:OPENCL_INCLUDE_DIRS = "C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v10.2\\include"

Write-Host $DIR

function check_command([string]$cmd) {
    Write-Host "Checking $($cmd)"
    if (-Not (Get-Command $cmd -errorAction SilentlyContinue)) {
        Write-Host "[failed]"
        $host.ui.WriteErrorLine("Missing command $($cmd)")
        exit -1
    } else {
    }
}

function configure() {
    check_command("cmake")

    New-Item -ItemType Directory -Force -Path ".\lib\Executor\build" | Out-Null
    New-Item -ItemType Directory -Force -Path ".\lib\Executor\build\install" | Out-Null
    Set-Location -Path ".\lib\Executor\build"
    Invoke-Expression ("cmake -DOPENCL_LIBRARIES='$Env:OPENCL_LIB_DIR/OpenCL.lib' " + `
        "-DOPENCL_INCLUDE_DIRS='$ENV:OPENCL_INCLUDE_DIRS' " + `
        # Bitness is to be configured manually here
        #"-DCMAKE_GENERATOR_PLATFORM=x64 " + `
        "-DCMAKE_INSTALL_PREFIX=$( $DIR )/src/main/resources ..")
}

function build() {
#    Invoke-Expression "cmake -G 'MinGW Makefiles' --target install --build .."
    Invoke-Expression "cmake --target install --build ."
    Invoke-Expression "MSBuild.exe Executor.sln"
}

Write-Host "Configure Executor"
configure

echo "Build Executor"
build

Set-Location -Path "$($DIR)"
