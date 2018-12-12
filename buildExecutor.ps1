$DIR = $PSScriptRoot

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
    Set-Location -Path ".\lib\Executor\build"
    Invoke-Expression ("cmake -DOPENCL_LIBRARIES=$Env:OPENCL_LIB_DIR/OpenCL.lib " + `
        "-DOPENCL_INCLUDE_DIRS=$ENV:OPENCL_INCLUDE_DIRS " + `
        # Bitness is to be configured manually here
        "-DCMAKE_GENERATOR_PLATFORM=x64 " + `
        "-DCMAKE_INSTALL_PREFIX=$( $DIR )/src/main/resources ..")
}

function build() {
    Invoke-Expression "cmake --build . --target install"
}

Write-Host "Configure Executor"
configure

echo "Build Executor"
build

Set-Location -Path "$($DIR)"