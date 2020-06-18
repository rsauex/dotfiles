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
