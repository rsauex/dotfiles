# ----- Path Context Diff -----

class PathContextAddition {
    [string]$Path
    $NewValue

    PathContextAddition($Path, $NewValue) {
        $this.Path = $Path
        $this.NewValue = $NewValue
        $this.LogOnApply()
    }

    [void] Revert() {
        Remove-Item $this.Path
        $this.LogOnRevert()
    }

    [void] LogOnApply() {
        Write-Host ("[PathContext:IN] {0} = {1}" -f $this.Path, $this.NewValue)
    }

    [void] LogOnRevert() {
        Write-Host ("[PathContext:OUT] {0} removed" -f $this.Path)
    }
}

class PathContextRemoval {
    [string]$Path
    $OldValue

    PathContextRemoval($Path, $OldValue) {
        $this.Path = $Path
        $this.OldValue = $OldValue
        $this.LogOnApply()
    }

    [void] Revert() {
        Set-Item $this.Path $this.OldValue
        $this.LogOnRevert()
    }

    [void] LogOnApply() {
        Write-Host ("[PathContext:IN] {0} removed" -f $this.Path)
    }

    [void] LogOnRevert() {
        Write-Host ("[PathContext:OUT] {0} = {1}" -f $this.Path, $this.OldValue)
    }
}

class PathContextChange {
    [string]$Path
    $OldValue
    $NewValue

    PathContextChange($Path, $OldValue, $NewValue) {
        $this.Path = $Path
        $this.OldValue = $OldValue
        $this.NewValue = $NewValue
        $this.LogOnApply()
    }

    [void] Revert() {
        Set-Item $this.Path $this.OldValue
        $this.LogOnRevert()
    }

    [void] LogOnApply() {
        Write-Host ("[PathContext:IN] {0} = {1}" -f $this.Path, $this.NewValue)
    }

    [void] LogOnRevert() {
        Write-Host ("[PathContext:OUT] {0} = {1}" -f $this.Path, $this.OldValue)
    }
}

class PathContextDiff {
    $Changes

    PathContextDiff([hashtable]$old, [hashtable]$new) {
        $this.Changes = @( )
        $old.GetEnumerator() | ForEach-Object {
            if (-not $new.ContainsKey($_.Key)) {
                $this.Changes += [PathContextRemoval]::new($_.Key, $_.Value)
            } elseif (-not $_.Value.Equals($new[$_.Key])) {
                $this.Changes += [PathContextChange]::new($_.Key, $_.Value, $new[$_.Key])
            }
        }
        $new.GetEnumerator() | ForEach-Object {
            if (-not $old.ContainsKey($_.Key)) {
                $this.Changes += [PathContextAddition]::new($_.Key, $_.Value)
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
        if ($null -ne $_.PSPath -and ($_.Options -notlike '*Constant*')) {
            $Dump[$_.PSPath] = (Get-Content -LiteralPath $_.PSPath)
        }
    }
    Get-ChildItem -Path Env: | ForEach-Object {
        $Dump[$_.PSPath] = (Get-Content -LiteralPath $_.PSPath)
    }
    Get-ChildItem -Path Function: | ForEach-Object {
        $Dump[$_.PSPath] = (Get-Content -LiteralPath $_.PSPath)
    }
    Get-ChildItem -Path Alias: | ForEach-Object {
        $Dump[$_.PSPath] = (Get-Content -LiteralPath $_.PSPath)
    }
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::$")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::?")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::^")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::_")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::args")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::CurrentlyExecutingCommand")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::Event")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::EventArgs")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::EventSubscriber")
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
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::PWD")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::Sender")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::StackTrace")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::switch")
    $Dump.Remove("Microsoft.PowerShell.Core\Variable::this")
    return $Dump
}

function Invoke-WithPathContextDiff([ScriptBlock[]]$Blocks) {
    $Old = (Get-EnvironmentDump)
    foreach ($Block in $Blocks) {
        Invoke-Command -ScriptBlock $Block
    }
    return [PathContextDiff]::new($Old, (Get-EnvironmentDump))
}

# ----- Utils -----

function Invoke-InTempEnv() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [ScriptBlock]$ScriptBlock
    )
    (Invoke-WithPathContextDiff($ScriptBlock)).Revert()
}

function Invoke-InPath() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [String]$Path,
        [Parameter(Mandatory=$true)]
        [ScriptBlock]$ScriptBlock
    )
    Push-Location
    try {
        Set-Location $Path
        Invoke-Command -ScriptBlock $ScriptBlock
    } finally {
        Pop-Location
    }
}

# ----- Context Path Hooks -----

[ScriptBlock[]]$PWSH_PATH_CONTEXT_HOOKS = @( )

function Add-PathContextHook() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [ScriptBlock]$Hook
    )
    $global:PWSH_PATH_CONTEXT_HOOKS += $Hook
}

function Invoke-PathContextHooks() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [String]$Path
    )
    Push-Location
    try {
        foreach ($hook in $PWSH_PATH_CONTEXT_HOOKS) {
            try {
                Set-Location $Path
                Invoke-Command -ScriptBlock $hook | Where-Object { $null -ne $_ } `
                  | ForEach-Object { $Block = $_; { Invoke-InPath $Path $Block }.GetNewClosure() }
            } catch {
                Write-Host "Error occured during execution of a PathContext hook:"
                Write-Host $_
                Write-Host $_.ScriptStackTrace
            }
        }
    } finally {
        Pop-Location
    }
}

# ----- Path Context Stack -----

$PWSH_PATH_CONTEXT_STACK = New-Object -TypeName 'Collections.Generic.LinkedList[PSCustomObject]'

function Pop-PathContext() {
    if ($null -ne $PWSH_PATH_CONTEXT_STACK.Last.Value.Diff) {
        $PWSH_PATH_CONTEXT_STACK.Last.Value.Diff.Revert()
    }
    $PWSH_PATH_CONTEXT_STACK.RemoveLast()
}

function Push-PathContext($PathPart, $Diff) {
    $PWSH_PATH_CONTEXT_STACK.AddLast([PSCustomObject]@{ Part = $PathPart; Diff = $Diff }) | Out-Null
}

function Get-PathContext() {
    $Drive = (Get-Location).Drive
    $PathContext = @( $Drive.Name )
    if ($Drive.CurrentLocation -ne "") {
        $PathContext = $PathContext + ($Drive.CurrentLocation.Split([IO.Path]::DirectorySeparatorChar))
    }
    return $PathContext
}

function Update-PathContextStack() {
    $PathContext = (Get-PathContext)
    # Find first index where path has changed
    $EndIndex = 0
    foreach ($location in $PWSH_PATH_CONTEXT_STACK.GetEnumerator()) {
        if ($location.Part -ne $PathContext[$EndIndex]) {
            break
        }
        $EndIndex++
    }
    # Skip everything else if nothing changed
    if ($PWSH_PATH_CONTEXT_STACK.Count -eq $EndIndex -and $PathContext.Count -eq $EndIndex) {
        return
    }
    # Remove the changed part from PathContext stack
    while ($PWSH_PATH_CONTEXT_STACK.Count -gt $EndIndex) {
        Pop-PathContext
    }
    # Bulid common part of the path
    $Drive = Get-PSDrive -Name $PathContext[0]
    $Path = $Drive.Root -ne "" ? $Drive.Root : ($Drive.Name + ":")
    for ($i = 1; $i -lt $EndIndex; $i++) {
        $Path = Join-Path $Path $PathContext[$i]
    }
    # Add new parts to PathContext stack
    for ($i = $EndIndex; $i -lt $PathContext.Count; $i++) {
        if ($i -ne 0) {
            $Path = Join-Path $Path $PathContext[$i]
        }
        $Blocks = (Invoke-PathContextHooks $Path | Where-Object { $null -ne $_ })
        Push-PathContext $PathContext[$i] ($Blocks.Count -ne 0 ? (Invoke-WithPathContextDiff $Blocks) : $null)
    }
}

Add-PromptHook {
    Update-PathContextStack
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

# ----- Path Context File -----

Set-Variable PathContextFileName -Option Constant -Value ".envrc.ps1"

# TODO: Use other way to auth files!

function Enable-PathContextFile() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [String[]]$Path
    )
    foreach ($p in $Path) {
        $File = Join-Path $p $PathContextFileName
        if (Test-Path $File -Type Leaf) {
            chmod +x "$File"
        }
    }
}
$PSDefaultParameterValues.Add("Enable-PathContextFile:Path", ".")

function Disable-PathContextFile() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [String[]]$Path
    )
    foreach ($p in $Path) {
        $File = Join-Path $p $PathContextFileName
        if (Test-Path $File -Type Leaf) {
            chmod -x "$File"
        }
    }
}
$PSDefaultParameterValues.Add("Disable-PathContextFile:Path", ".")

Function Test-PathContextFile() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true, ValueFromPipeline=$true)]
        [String[]]$Path
    )
    foreach ($p in $Path) {
        $File = Join-Path $p $PathContextFileName
        if (-not (Test-Path $File -Type Leaf)) {
            return $false
        }
        if (([int]((stat -c "%a" "$File")[0]) -band 1) -eq 0) {
            Write-Host "Path Context File in $PWD is disabled! Use Enable-PathContextFile cmdlet to enable."
            return $false
        }
    }
    return $true
}
$PSDefaultParameterValues.Add("Test-PathContextFile:Path", ".")

Add-PathContextHook {
    if (Test-PathContextFile) {
        $Path = Join-Path -Path $PWD -ChildPath $PathContextFileName
        return { . "$Path" }.GetNewClosure()
    }
    return $null
}
