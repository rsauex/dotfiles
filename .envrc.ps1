$script:GUIX_CONFIG_HOME = "${PWD}"

$Env:GUILE_LOAD_PATH = "${GUIX_CONFIG_HOME}:${Env:GUILE_LOAD_PATH}"

function global:Update-Guix() {
    [CmdletBinding()]
    param(
        [switch]$AllowDowngrades
    )

    $final_args = @(
        "-C", "${GUIX_CONFIG_HOME}/rsauex/channels.scm"
    )
    if ($AllowDowngrades) {
        $final_args += "--allow-downgrades"
    }

    Write-Host "Updating Guix"
    guix pull $final_args
}

function global:Build-GuixSystem() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory = $true, Position = 0)]
        [ValidatePattern("[a-zA-Z][a-zA-Z0-9-]*")]
        [String]$Machine,

        [switch]$AllowDowngrades
    )

    $final_args = @(
        "-e", "(@ (rsauex systems pc ${machine}) %os)"
    )
    if ($AllowDowngrades) {
        $final_args += "--allow-downgrades"
    }

    Write-Host "Building system for host '${machine}'"
    sudo -E guix system reconfigure $final_args
}

$global:PSDefaultParameterValues["Build-GuixSystem:Machine"] = { [Environment]::MachineName }

function global:Build-GuixHome() {
    [CmdletBinding()]
    param(
        [switch]$AllowDowngrades
    )

    $final_args = @(
        "-e", "((@ (rsauex home) %home-environment))"
    )
    if ($AllowDowngrades) {
        $final_args += "--allow-downgrades"
    }

    Write-Host "Building home"
    guix home reconfigure $final_args
}

function global:Clear-Guix() {
    param()
    sudo guix system delete-generations
    guix home delete-generations
    guix pull --delete-generations
    sudo guix gc -d
}
