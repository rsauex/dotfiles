function Test-LoginShell() {
    (ps -p $PID -o cmd=) -match "^-pwsh"
}

# ----- Path Alias -----

[hashtable]$PWSH_PATH_ALIASES = @{ }

function Set-PathAlias() {
    [CmdletBinding()]
    param(
        [string]$path,
        [string]$alias
    )
    $PWSH_PATH_ALIASES[$path] = $alias
}

function Convert-PathAlias() {
    [CmdletBinding()]
    param(
        [string]$path
    )
    $path_parts = $path.split('/')
    for($i = $path_parts.count - 1; $i -gt 0; $i--) {
        $current_subpath = $path_parts[0..$i] -Join '/'
        if ($PWSH_PATH_ALIASES.ContainsKey($current_subpath)) {
            return (@( $PWSH_PATH_ALIASES[$current_subpath] ) + $path_parts[($i + 1)..$path_parts.count]) -Join '/'
        }
    }
    return $path
}

Set-PathAlias "/home/$env:USER" '~'

# ----- Prompt -----

[ScriptBlock[]]$PWSH_PROMPT_HOOKS = @( )

<#
.Synopsis
Add a hook to Prompt function.
.Description
Add a hook which will be executed every time the Prompt function is executed.
All the hooks will be executed before actual prompt is shown.
.Parameter Hook
Hook to be executed.
#>
function Add-PromptHook() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [ScriptBlock]$Hook
    )
    $global:PWSH_PROMPT_HOOKS += $Hook
}

function Prompt {
    # Invoke hooks
    $PWSH_PROMPT_HOOKS | ForEach { Invoke-Command -ScriptBlock $_ }

    # Output actual prompt
    Write-Host "PS>" -NoNewline
    Write-Host " " -NoNewline
    Write-Host ("{0:HH:mm}" -f (Get-Date)) -NoNewline -ForegroundColor Cyan
    Write-Host " " -NoNewline
    Write-Host "[" -NoNewline
    Write-Host ("{0}@{1}" -f $env:USER, (hostname)) -NoNewline -ForegroundColor Blue
    Write-Host " " -NoNewline
    Write-Host ("+{0}" -f $env:SHLVL) -NoNewline -ForegroundColor Green
    Write-Host "]" -NoNewline
    Write-Host " " -NoNewline
    Write-Host "$(Convert-PathAlias $pwd.path)" -NoNewline -ForegroundColor White
    Write-Host ""
    ">> "
}

# ----- Tmux -----

Add-PromptHook {
    if (-Not (Test-Path env:TMUX)) {
        return
    }
    foreach ($line in (tmux show-environment)) {
        switch -Regex ($line) {
            '^([a-zA-Z0-9_]+)=(.*)$' {
                Set-Item -Path Env:$matches[1] -Value $matches[2]
            }
            '^-([a-zA-Z0-9_]+)$' {
                Remove-Item -Path Env:$matches[1]
            }
        }
    }
}
