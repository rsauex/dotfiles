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
    $PWSH_PROMPT_HOOKS | ForEach-Object { Invoke-Command -ScriptBlock $_ }

    ## Output actual prompt
    # First line
    Write-Host "PS> " -NoNewline
    Write-Host ("{0:HH:mm}" -f (Get-Date)) -NoNewline -ForegroundColor Cyan
    Write-Host " [" -NoNewline
    Write-Host ("{0}@{1}" -f $env:USER, (hostname)) -NoNewline -ForegroundColor Blue
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
                Set-Item -Path Env:$matches[1] -Value $matches[2]
            }
            '^-([a-zA-Z0-9_]+)$' {
                Remove-Item -Path Env:$matches[1]
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

# ----- DirEnv -----

class DirEnvAddition {
    [string]$Path

    DirEnvAddition($Path) {
        $this.Path = $Path
    }

    [void] Revert() {
        Remove-Item $this.Path
    }
}

class DirEnvChange {
    [string]$Path
    $OldValue

    DirEnvChange($Path, $OldValue) {
        $this.Path = $Path
        $this.OldValue = $OldValue
    }

    [void] Revert() {
        Set-Item $this.Path $this.OldValue
    }
}

class DirEnvDiff {
    $Changes

    DirEnvDiff([hashtable]$old, [hashtable]$new) {
        $this.Changes = @( )
        $old.GetEnumerator() | ForEach-Object {
            if (-not $new.ContainsKey($_.Key)) {
                $this.Changes += [DirEnvChange]::new($_.Key, $_.Value)
            } elseif (-not $_.Value.Equals($new[$_.Key])) {
                $this.Changes += [DirEnvChange]::new($_.Key, $_.Value)
            }
        }
        $new.GetEnumerator() | ForEach-Object {
            if (-not $old.ContainsKey($_.Key)) {
                $this.Changes += [DirEnvAddition]::new($_.Key)
            }
        }
    }

    [void] Revert() {
        foreach ($Change in $this.Changes) {
            $Change.Revert()
        }
    }
}

function Get-EnvironmentDump() {
    [hashtable]$Dump = @{ }
    Get-Variable -Scope 'global' | ForEach-Object {
        if ($null -ne $_.PSPath) {
            $Dump[$_.PSPath] = (Get-Content $_.PSPath)
        }
    }
    Get-ChildItem -Path Env: | ForEach-Object {
        $Dump[$_.PSPath] = (Get-Content $_.PSPath)
    }
    Get-ChildItem -Path Function: | ForEach-Object {
        $Dump[$_.PSPath] = (Get-Content $_.PSPath)
    }
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::$")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::?")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::^")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::_")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::args")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::CurrentlyExecutingCommand")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::Error")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::Event")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::EventArgs")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::EventSubscriber")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::ExecutionContext")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::false")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::foreach")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::input")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::LastExitCode")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::Matches")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::MyInvocation")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::NestedPromptLevel")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::null")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::PSBoundParameters")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::PSCmdlet")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::PSCommandPath")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::PSCulture")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::PSDebugContext")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::PSItem")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::PSScriptRoot")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::PSSenderInfo")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::PSUICulture")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::PSVersionTable")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::PWD")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::Sender")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::ShellId")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::StackTrace")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::switch")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::this")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::true")
    return $Dump
}

function Invoke-WithDiff([ScriptBlock[]]$Blocks) {
    $Old = (Get-EnvironmentDump)
    foreach ($Block in $Blocks) {
        Invoke-Command -ScriptBlock $Block
    }
    return [DirEnvDiff]::new($Old, (Get-EnvironmentDump))
}

[ScriptBlock[]]$PWSH_PATH_CONTEXT_HOOKS = @( )

function Add-PathContextHook() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [ScriptBlock]$Hook
    )
    $global:PWSH_PATH_CONTEXT_HOOKS += $Hook
}

$PWSH_PATH_CONTEXT_STACK = New-Object -TypeName 'Collections.Generic.LinkedList[PSCustomObject]'

function Get-PathContext() {
    $Drive = (Get-Location).Drive
    return @( $Drive.Name ) + ($Drive.CurrentLocation.Split([IO.Path]::DirectorySeparatorChar))
}

function Update-PathContextStack() {
    $PathContext = (Get-PathContext)
    $EndIndex = 0
    foreach ($location in $PWSH_PATH_CONTEXT_STACK.GetEnumerator()) {
        if ($location.Part -ne $PathContext[$EndIndex]) {
            break
        }
        $EndIndex++
    }
    while ($PWSH_PATH_CONTEXT_STACK.Count -gt $EndIndex) {
        if ($null -ne $PWSH_PATH_CONTEXT_STACK.Last.Value) {
            $PWSH_PATH_CONTEXT_STACK.Last.Value.Diff.Revert()
        }
        $PWSH_PATH_CONTEXT_STACK.RemoveLast()
    }
    $Drive = (Get-PSDrive -Name $PathContext[0])
    $Path = $Drive.Root -ne "" ? $Drive.Root : ($Drive.Name + ":")
    for ($i = 1; $i -lt $EndIndex; $i++) {
        $Path = Join-Path -Path $Path -ChildPath $PathContext[$i]
    }
    for ($i = $EndIndex; $i -lt $PathContext.Count; $i++) {
        $Diff = $null
        if ($i -ne 0) {
            $Path = Join-Path -Path $Path -ChildPath $PathContext[$i]
            $Blocks = ($PWSH_PATH_CONTEXT_HOOKS | ForEach-Object { Invoke-Command -ScriptBlock $_ -ArgumentList $Path } | Where-Object { $null -ne $_ })
            if ($Blocks.Count -ne 0) {
                $Diff = (Invoke-WithDiff $Blocks)
            }
        }
        $PWSH_PATH_CONTEXT_STACK.AddLast([PSCustomObject]@{ Part = $PathContext[$i]; Diff = $Diff })
    }
    return $null
}

Add-PromptHook {
    Update-PathContextStack | Out-Null # TODO: Remove Out-Null
}

# Add-PathContextHook {
#     Write-Host $args[0]
#     return $null
# }

# Add-PathContextHook {
#     if (Test-Path (Join-Path -Path $args[0] -ChildPath ".git")) {
#         return { Set-Content Env:/IS_IN_GIT_TEST 'TRUE' }
#     }
#     return $null
# }
