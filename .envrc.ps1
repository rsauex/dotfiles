$script:GUIX_CONFIG_HOME = "${PWD}"

$Env:GUILE_LOAD_PATH = "${GUIX_CONFIG_HOME}:${Env:GUILE_LOAD_PATH}"

function global:Build-GuixSystem() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory = $true, Position = 0)]
        [ValidatePattern("[a-zA-Z][a-zA-Z0-9-]*")]
        [String]$Machine
    )
    Write-Host "Building system for host '${machine}'"
    sudo -E guix system reconfigure -e "(@ (rsauex systems pc ${machine}) %os)"
}

$global:PSDefaultParameterValues["Build-GuixSystem:Machine"] = { [Environment]::MachineName }

function global:Build-GuixHome() {
    [CmdletBinding()]
    param()
    Write-Host "Building home"
    guix home reconfigure -e "(@ (rsauex home) %home-environment)"
}

function global:Clear-Guix() {
    param()
    sudo guix system delete-generations
    guix home delete-generations
    guix pull --delete-generations
    sudo guix gc -d
}
