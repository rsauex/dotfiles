# TODO: PathContextHook -> PathContextEnterHook
# TODO: Create PathContextExitHook (+ run in oposite order for proper nesting)

# ----- Header -----

# TODO: Proper exports
# TODO: Split into different files by logical parts

Set-StrictMode -Verbose -Version Latest

# ----- Stored value -----

# TODO: Ideally, providers should be used instead

class StoredEnvVariable {
    [String]$Name
    [Object]$Value

    StoredEnvVariable([String]$Name) {
        $this.Name = $Name
        $this.Value = $this.GetCurrentValue()
    }

    [Object]GetCurrentValue() {
        return [Environment]::GetEnvironmentVariable($this.Name)
    }

    [Void]SetCurrentValue($Value) {
        [Environment]::SetEnvironmentVariable($this.Name, $Value)
    }

    [Object]GetValue() {
        return $this.Value
    }

    [Void]Restore() {
        $this.SetCurrentValue($this.Value)
    }

    [Void]Remove() {
        $this.SetCurrentValue($null)
    }

    [Void]Checkpoint() {
        # Do Nothing
    }
}

class StoredPwshVariable {
    static [Hashtable]$IGNORED_VARS = @{
        'PWD' = $true
        '^' = $true
        '$' = $true
        'args' = $true
        'input' = $true
        'null' = $true
        'LASTEXITCODE' = $true
        'MyInvocation' = $true
        'NestedPromptLevel' = $true
        'StackTrace' = $true
    }

    [String]$Scope
    [String]$Name
    [Object]$Value

    StoredPwshVariable([String]$Scope, [String]$Name) {
        $this.Scope = $Scope
        $this.Name = $Name
        $this.Value = $this.GetCurrentValue()
    }

    [Object]GetCurrentValue() {
        return (Get-Variable -Scope $this.Scope -Name $this.Name).Value
    }

    [Void]SetCurrentValue($Value) {
        Set-Variable -Scope $this.Scope -Name $this.Name -Value $Value
    }

    [Object]GetValue() {
        return $this.Value
    }

    [Void]Restore() {
        $this.SetCurrentValue($this.Value)
    }

    [Void]Remove() {
        Remove-Variable -Scope $this.Scope -Name $this.Name
    }

    [Void]Checkpoint() {
        if ($this.Value -is [ICloneable]) {
            $this.SetCurrentValue($this.Value.Clone())
        }
    }
}

class StoredAlias {
    [String]$Scope
    [String]$Name
    [Object]$Value

    StoredAlias([String]$Scope, [String]$Name) {
        $this.Scope = $Scope
        $this.Name = $Name
        $this.Value = $this.GetCurrentValue()
    }

    [Object]GetCurrentValue() {
        return (Get-Alias -Scope $this.Scope -Name $this.Name).Definition
    }

    [Void]SetCurrentValue($Value) {
        Set-Alias -Scope $this.Scope -Name $this.Name -Value $Value
    }

    [Object]GetValue() {
        return $this.Value
    }

    [Void]Restore() {
        $this.SetCurrentValue($this.Value)
    }

    [Void]Remove() {
        Remove-Alias -Scope $this.Scope -Name $this.name
    }

    [Void]Checkpoint() {
        # Do Nothing
    }
}

class StoredFunction {
    [String]$PSPath
    [Object]$Value

    StoredFunction([String]$Scope, [String]$Name) {
        $this.PSPath = "Microsoft.PowerShell.Core\Function::" + $Name
        $this.Value = $this.GetCurrentValue()
    }

    [Object]GetCurrentValue() {
        return (Get-Content -LiteralPath $this.PSPath)
    }

    [Void]SetCurrentValue($Value) {
        Set-Content -LiteralPath $this.PSPath -Value $Value
    }

    [Object]GetValue() {
        return $this.Value
    }

    [Void]Restore() {
        $this.SetCurrentValue($this.Value)
    }

    [Void]Remove() {
        Remove-Item -LiteralPath $this.PSPath
    }

    [Void]Checkpoint() {
        # Do Nothing
    }
}

function Get-Environment() {
    $Vars = @{}
    [Environment]::GetEnvironmentVariables().Keys | ForEach-Object {
        $Vars["Env Var: " + $_] = [StoredEnvVariable]::new($_)
    }
    Get-Variable -Scope Global | ForEach-Object {
        if ($_.Options -notlike '*Constant*' -and
            $_.Options -notlike '*AllScope*' -and
            -not [StoredPwshVariable]::IGNORED_VARS.Contains($_.Name)) {
            $Vars["Global Var: " + $_.Name] = [StoredPwshVariable]::new('Global', $_.name)
        }
    }
    Get-Alias -Scope Global | ForEach-Object {
        if ($_.Options -notlike '*AllScope*') {
            $Vars["Global Alias: " + $_.Name] = [StoredAlias]::new('Global', $_.Name)
        }
    }
    Get-ChildItem -Path Function: | ForEach-Object {
        $Vars["Global Function: " + $_.Name] = [StoredFunction]::new('Global', $_.Name)
    }
    return $Vars
}

function Checkpoint-Environment() {
    $Env = (Get-Environment)
    $Env.Values | ForEach-Object {
        $_.Checkpoint()
    }
    return $Env
}

# ----- Path Context Diff -----

class EnvironmentAddition {
    [String]$Path
    [Object]$NewEntity

    EnvironmentAddition([String]$Path, [Object]$NewEntity) {
        $this.Path = $Path
        $this.NewEntity = $NewEntity
        $this.LogOnApply()
    }

    [Void]Revert() {
        $this.NewEntity.Remove()
        $this.LogOnRevert()
    }

    [Void]LogOnApply() {
        Write-Host ("[PathContext:IN] {0} = {1}" -f $this.Path, $this.NewEntity.GetValue())
    }

    [Void]LogOnRevert() {
        Write-Host ("[PathContext:OUT] {0} removed" -f $this.Path)
    }
}

class EnvironmentRemoval {
    [String]$Path
    [Object]$OldEntity

    EnvironmentRemoval([String]$Path, [Object]$OldEntity) {
        $this.Path = $Path
        $this.OldEntity = $OldEntity
        $this.LogOnApply()
    }

    [Void]Revert() {
        $this.OldEntity.Restore()
        $this.LogOnRevert()
    }

    [Void]LogOnApply() {
        Write-Host ("[PathContext:IN] {0} removed" -f $this.Path)
    }

    [Void]LogOnRevert() {
        Write-Host ("[PathContext:OUT] {0} = {1}" -f $this.Path, $this.OldEntity.GetValue())
    }
}

class EnvironmentChange {
    [String]$Path
    [Object]$OldEntity
    [Object]$NewEntity

    EnvironmentChange([String]$Path, [Object]$OldEntity, [Object]$NewEntity) {
        $this.Path = $Path
        $this.OldEntity = $OldEntity
        $this.NewEntity = $NewEntity
        $this.LogOnApply()
    }

    [Void]Revert() {
        $this.OldEntity.Restore()
        $this.LogOnRevert()
    }

    [Void]LogOnApply() {
        Write-Host ("[PathContext:IN] {0} = {1}" -f $this.Path, $this.NewEntity.GetValue())
    }

    [Void]LogOnRevert() {
        Write-Host ("[PathContext:OUT] {0} = {1}" -f $this.Path, $this.OldEntity.GetValue())
    }
}

class EnvironmentDiff {
    [Object[]]$Changes

    EnvironmentDiff([Hashtable]$old, [Hashtable]$new) {
        $this.Changes = @( )
        $old.GetEnumerator() | ForEach-Object {
            if (-not $new.ContainsKey($_.Key)) {
                $this.Changes += [EnvironmentRemoval]::new($_.Key, $_.Value)
            } elseif ($_.Value.GetValue() -ne $new[$_.Key].GetValue() -and
                      ($null -eq $_.Value.GetValue() -or
                       $null -eq $new[$_.Key].GetValue() -or
                       -not $_.Value.GetValue().Equals($new[$_.Key].GetValue()))) {
                $this.Changes += [EnvironmentChange]::new($_.Key, $_.Value, $new[$_.Key])
            }
        }
        $new.GetEnumerator() | ForEach-Object {
            if (-not $old.ContainsKey($_.Key)) {
                $this.Changes += [EnvironmentAddition]::new($_.Key, $_.Value)
            }
        }
    }

    [Void]Revert() {
        foreach ($Change in $this.Changes) {
            $Change.Revert()
        }
    }
}

function Invoke-WithEnvironmentDiff([ScriptBlock[]]$Blocks) {
    $Old = (Checkpoint-Environment)
    foreach ($Block in $Blocks) {
        Invoke-Command -ScriptBlock $Block
    }
    return [EnvironmentDiff]::new($Old, (Get-Environment))
}

# ----- Utils -----

function Invoke-Using() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [Object]$Object,
        [Parameter(Mandatory=$true)]
        [ScriptBlock]$ScriptBlock
    )
    try {
        Invoke-Command -ScriptBlock $ScriptBlock
    } finally {
        $Object.Dispose()
    }
}

class TemporaryEnvironment {
    [Hashtable]$Old

    TemporaryEnvironment() {
        $this.Old = (Checkpoint-Environment)
    }

    [Void]Dispose() {
        [EnvironmentDiff]::new($this.Old, (Get-Environment)).Revert()
    }
}

function Invoke-InTempEnv() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [ScriptBlock]$ScriptBlock
    )
    Invoke-Using ([TemporaryEnvironment]::new()) {
        Invoke-Command -ScriptBlock $ScriptBlock
    }.GetNewClosure()
}

class TemporaryPath {
    [String]$Old

    TemporaryPath() {
        $this.Old = $PWD
    }

    [Void]Dispose() {
        Set-Location $this.Old
    }
}

function Invoke-InPath() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [String]$Path,
        [Parameter(Mandatory=$true)]
        [ScriptBlock]$ScriptBlock
    )
    Invoke-Using ([TemporaryPath]::new()) {
        Set-Location $Path
        Invoke-Command -ScriptBlock $ScriptBlock
    }.GetNewClosure()
}

# ----- Context Path Hooks -----

[ScriptBlock[]]$script:PWSH_PATH_CONTEXT_HOOKS = @( )

function Add-PathContextHook() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [ScriptBlock]$Hook
    )
    $script:PWSH_PATH_CONTEXT_HOOKS += $Hook
}

function Invoke-PathContextHooks() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [String]$Path
    )
    Push-Location
    try {
        foreach ($hook in $script:PWSH_PATH_CONTEXT_HOOKS) {
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

$script:PWSH_PATH_CONTEXT_STACK = New-Object -TypeName 'Collections.Generic.LinkedList[PSCustomObject]'

function Pop-PathContext() {
    if ($null -ne $script:PWSH_PATH_CONTEXT_STACK.Last.Value.Diff) {
        $script:PWSH_PATH_CONTEXT_STACK.Last.Value.Diff.Revert()
    }
    $script:PWSH_PATH_CONTEXT_STACK.RemoveLast()
}

function Push-PathContext($PathPart, $Diff) {
    $script:PWSH_PATH_CONTEXT_STACK.AddLast([PSCustomObject]@{ Part = $PathPart; Diff = $Diff }) | Out-Null
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
    foreach ($location in $script:PWSH_PATH_CONTEXT_STACK.GetEnumerator()) {
        if ($PathContext.Count -eq $EndIndex -or $location.Part -ne $PathContext[$EndIndex]) {
            break
        }
        $EndIndex++
    }
    # Skip everything else if nothing changed
    if ($script:PWSH_PATH_CONTEXT_STACK.Count -eq $EndIndex -and $PathContext.Count -eq $EndIndex) {
        return
    }
    # Remove the changed part from PathContext stack
    while ($script:PWSH_PATH_CONTEXT_STACK.Count -gt $EndIndex) {
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
        $Blocks = @(Invoke-PathContextHooks $Path | Where-Object { $null -ne $_ })
        Push-PathContext $PathContext[$i] ($Blocks.Count -ne 0 ? (Invoke-WithEnvironmentDiff $Blocks) : $null)
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

Set-Variable -Scope Script -Name PathContextFileName -Option Constant -Value ".envrc.ps1"

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
    [OutputType([System.Boolean])]
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
