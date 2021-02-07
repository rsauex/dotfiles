$script:GUIX_CONFIG_HOME = "${PWD}"

$Env:GUILE_LOAD_PATH = "${GUIX_CONFIG_HOME}:${Env:GUILE_LOAD_PATH}"

function global:Build-GuixSystem() {
    [CmdletBinding()]
    param()

    sudo -E guix system reconfigure (Join-Path $script:GUIX_CONFIG_HOME "config.scm")
}
