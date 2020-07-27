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
    $path_parts = $path.split([IO.Path]::DirectorySeparatorChar)
    for($i = $path_parts.count - 1; $i -gt 0; $i--) {
        $current_subpath = $path_parts[0..$i] -Join [IO.Path]::DirectorySeparatorChar
        if ($PWSH_PATH_ALIASES.ContainsKey($current_subpath)) {
            return (@( $PWSH_PATH_ALIASES[$current_subpath] ) + $path_parts[($i + 1)..$path_parts.count]) -Join [IO.Path]::DirectorySeparatorChar
        }
    }
    return $path
}

Set-PathAlias "$HOME" '~'

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

function Invoke-PromptHooks() {
    foreach ($hook in $PWSH_PROMPT_HOOKS) {
        try {
            Invoke-Command -ScriptBlock $hook
        } catch {
            Write-Host "Error occured during execution of a prompt hook:"
            Write-Host $_
            Write-Host $_.ScriptStackTrace
        }
    }
}

$env:SHLVL = [int]$env:SHLVL + 1

function Prompt {
    # Invoke hooks
    Invoke-PromptHooks

    ## Output actual prompt
    # First line
    Write-Host "PS> " -NoNewline
    Write-Host ("{0:HH:mm}" -f (Get-Date)) -NoNewline -ForegroundColor Cyan
    Write-Host " [" -NoNewline
    Write-Host ("{0}@{1}" -f [System.Environment]::UserName, [System.Environment]::MachineName) -NoNewline -ForegroundColor Blue
    Write-Host (":{0}" -f ($env:SSH_TTY ?? 'o')) -NoNewline -ForegroundColor DarkGray
    Write-Host " " -NoNewline
    Write-Host ("+{0}" -f $env:SHLVL) -NoNewline -ForegroundColor Green
    Write-Host "] " -NoNewline
    Write-Host $(Convert-PathAlias $pwd.path) -NoNewline -ForegroundColor White
    Write-Host " " -NoNewline
    # Separator line
    Write-Host ('─' * ([Console]::WindowWidth - [Console]::CursorLeft)) -ForegroundColor DarkGray
    # Secord Line
    " $ "
}

# ----- Tmux -----

Add-PromptHook {
    if (-not (Test-Path env:TMUX)) {
        return
    }
    foreach ($line in (tmux show-environment)) {
        switch -Regex ($line) {
            '^([a-zA-Z0-9_]+)=(.*)$' {
                Set-Content -Path Env:/$($matches[1]) -Value $matches[2]
            }
            '^-([a-zA-Z0-9_]+)$' {
                Remove-Item -ErrorAction:Ignore -Path Env:/$($matches[1])
            }
        }
    }
}

# ----- Utils -----

function Find-String() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [String]$Pattern,

        [Parameter(Mandatory=$false)]
        [String[]]$Path,

        [Parameter(Mandatory=$false)]
        [Switch]$CaseSensitive
    )

    $arguments = @( )
    if ($CaseSensitive) {
        $arguments += '-s'
    }
    [string[]]$DefaultProperties = 'path', 'lines'
    $ddps = New-Object System.Management.Automation.PSPropertySet DefaultDisplayPropertySet, $DefaultProperties
    $PSStandardMembers = [System.Management.Automation.PSMemberInfo[]]$ddps

    (&rg --json $arguments $pattern $path) | ConvertFrom-Json | Where-Object { $_.type -eq 'match' } `
      | ForEach-Object { $_.data } | Add-Member -PassThru -MemberType MemberSet -Name PSStandardMembers -Value $PSStandardMembers
}

function New-Link() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [String[]]$Path,

        [Parameter(Mandatory=$true)]
        [String]$Target,

        [Switch]$Hard
    )
    New-Item -Path $Path -ItemType ($Hard ? "HardLink" : "SymbolicLink") -Value $Target
}

# ----- PathContext -----

. (Join-Path (Split-Path -Parent $PROFILE) "PathContext.ps1")
