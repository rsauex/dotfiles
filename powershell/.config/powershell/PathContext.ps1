class PathContextAddition {
    [string]$Path

    PathContextAddition($Path) {
        $this.Path = $Path
    }

    [void] Revert() {
        Remove-Item $this.Path
    }
}

class PathContextChange {
    [string]$Path
    $OldValue

    PathContextChange($Path, $OldValue) {
        $this.Path = $Path
        $this.OldValue = $OldValue
    }

    [void] Revert() {
        Set-Item $this.Path $this.OldValue
    }
}

class PathContextDiff {
    $Changes

    PathContextDiff([hashtable]$old, [hashtable]$new) {
        $this.Changes = @( )
        $old.GetEnumerator() | ForEach-Object {
            if (-not $new.ContainsKey($_.Key)) {
                $this.Changes += [PathContextChange]::new($_.Key, $_.Value)
            } elseif (-not $_.Value.Equals($new[$_.Key])) {
                $this.Changes += [PathContextChange]::new($_.Key, $_.Value)
            }
        }
        $new.GetEnumerator() | ForEach-Object {
            if (-not $old.ContainsKey($_.Key)) {
                $this.Changes += [PathContextAddition]::new($_.Key)
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
    return [PathContextDiff]::new($Old, (Get-EnvironmentDump))
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

function Invoke-PathContextHooks() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [Object[]]$ArgumentList
    )
    foreach ($hook in $PWSH_PATH_CONTEXT_HOOKS) {
        try {
            Invoke-Command -ScriptBlock $hook -ArgumentList @ArgumentList
        } catch {
            Write-Host "Error occured during execution of a PathContext hook:"
            Write-Host $_
            Write-Host $_.ScriptStackTrace
        }
    }
}

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
        Push-PathContext $PathContext[$i] ($Blocks.Count -ne 0 ? (Invoke-WithDiff $Blocks) : $null)
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

Add-PathContextHook {
    $Path = Join-Path -Path $args[0] -ChildPath ".envrc.ps1"
    if (Test-Path $Path -Type Leaf) {
        # TODO: Use other way to auth files!
        if (([int]((stat -c "%a" "$Path")[0]) -band 1) -eq 1) {
            return { . "$Path" }.GetNewClosure()
        }
    }
    return $null
}
