function Test-LoginShell() {
    (ps -p $PID -o cmd=) -match "^-pwsh"
}

function Test-InDockerContainer() {
    return Test-Path "/.dockerenv"
}

# ------------------------------------------------------------------------------
# ----- Custom formatters ------------------------------------------------------

Update-FormatData -PrependPath (Join-Path (Split-Path -Parent $PROFILE) "formatters" "*.Format.ps1xml")

# ------------------------------------------------------------------------------
# ----- ReadLine options -------------------------------------------------------

# Disable Predictive IntelliSense
Set-PSReadLineOption -PredictionSource None

Set-PSReadLineOption -Colors @{
  Parameter = [System.ConsoleColor]::DarkMagenta
  Operator  = [System.ConsoleColor]::DarkMagenta
}

# Don't save commands starting with a semicolon into history
Set-PSReadLineOption -AddToHistoryHandler {
    param([string]$line)
    return $line.Length -gt 3 -and $line[0] -ne ';'
}

# ------------------------------------------------------------------------------
# ----- Path Alias -------------------------------------------------------------

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

# ------------------------------------------------------------------------------
# ----- Prompt Hooks -----------------------------------------------------------

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

# ------------------------------------------------------------------------------
# ----- Prompt -----------------------------------------------------------------

$env:SHLVL = [int]$env:SHLVL + 1

function Prompt {
    # Invoke hooks
    Invoke-PromptHooks

    ## Maybe output last exit code
    if ($null -ne $LastExitCode -and $LastExitCode -ne 0) {
        Write-Host -ForegroundColor Green ("Exit code: {0}" -f $LastExitCode)
        # TODO: Any better way?
        $global:LastExitCode = $null
    }

    ## Output actual prompt
    # First line
    $indicators = ""
    if (Test-InDockerContainer) {
        $indicators += "(DOCKER)"
    }
    if (Test-Path Env:/IN_NIX_SHELL) {
        $indicators += "(NIX)"
    }
    if (Test-Path Env:/GUIX_ENVIRONMENT) {
        $indicators += "(GUIX)"
    }
    Write-Host -NoNewline                           -Object "PS${indicators}> "
    Write-Host -NoNewline -ForegroundColor Cyan     -Object ("{0:HH:mm}" -f (Get-Date))
    Write-Host -NoNewline                           -Object " ["
    Write-Host -NoNewline -ForegroundColor Blue     -Object ("{0}@{1}" -f [System.Environment]::UserName, [System.Environment]::MachineName)
    Write-Host -NoNewline -ForegroundColor DarkGray -Object (":{0}" -f ($env:SSH_TTY ?? 'o'))
    Write-Host -NoNewline                           -Object " "
    Write-Host -NoNewline -ForegroundColor Green    -Object ("+{0}" -f $env:SHLVL)
    Write-Host -NoNewline                           -Object "] "
    Write-Host -NoNewline -ForegroundColor White    -Object $(Convert-PathAlias $pwd.path)
    Write-Host -NoNewline                           -Object " "
    # Separator line
    Write-Host            -ForegroundColor DarkGray -Object ('─' * ([Console]::WindowWidth - [Console]::CursorLeft))
    # Secord Line
    " $ "
}

# Display actual info in window title
Add-PromptHook {
    $Host.UI.RawUI.WindowTitle = ("{0}@{1}:{2}" -f [System.Environment]::UserName, [System.Environment]::MachineName, $pwd.path)
}

# ------------------------------------------------------------------------------
# ----- Tmux -------------------------------------------------------------------

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

# ------------------------------------------------------------------------------
# ----- Utils ------------------------------------------------------------------

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

<#
.Synopsis
Modify content using some script block.
.Description
Get content by Path, pass it to the script block by pipe, set the result
back to Path
.Parameter Path
Path of the item to be modified.
.Parameter Raw
Whether split content into lines.
.Parameter ScriptBlock
Script block which should be applied to the content
#>
function Edit-Content() {
    [CmdletBinding(PositionalBinding = $False)]
    param(
        [Parameter(Position = 0, ValueFromPipeline = $true)]
        [String]$Path,

        [Parameter(Mandatory = $true, Position = 1)]
        [ScriptBlock]$ScriptBlock,

        [Parameter()]
        [Switch]$Raw
    )
    Get-Content -Raw:$Raw -LiteralPath $Path
    | Foreach-Object { Invoke-Command -ScriptBlock $ScriptBlock }
    | Set-Content -LiteralPath $Path
}

<#
.Synopsis
Select N first elements from the stream.
.Description
Same as `Select-Object -First $Count`.
.Parameter Count
Number of elements to select.
#>
function Select-First() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory = $true)]
        [int]$Count,

        [Parameter(Mandatory = $true, ValueFromPipeline = $true)]
        [object[]]$InputObject
    )
    $input | Select-Object -First $Count
}

<#
.Synopsis
Select N last elements from the stream.
.Description
Same as `Select-Object -Last $Count`.
.Parameter Count
Number of elements to select.
#>
function Select-Last() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory = $true)]
        [int]$Count,

        [Parameter(Mandatory = $true, ValueFromPipeline = $true)]
        [object[]]$InputObject
    )
    $input | Select-Object -Last $Count
}

# ------------------------------------------------------------------------------
# ----- PathContext ------------------------------------------------------------

Import-Module -Name (Join-Path (Split-Path -Parent $PROFILE) "PathContext.psm1")

Add-PromptHook {
    Update-PathContextStack
}

# ------------------------------------------------------------------------------
# ----- Aliases ----------------------------------------------------------------

New-Alias -Name 'sc'  -Value Set-Content
New-Alias -Name 'edc' -Value Edit-Content

# ------------------------------------------------------------------------------
# ----- Watch ------------------------------------------------------------------

function Watch-Command() {
    [CmdletBinding(PositionalBinding = $False)]
    param(
        [Parameter(Position = 0, Mandatory = $true)]
        [ScriptBlock]$ScriptBlock,

        [Parameter()]
        [Double]$Seconds
    )

    Write-Host -NoNewline "`e[2J"
    Write-Host -NoNewline "`e[1;1H"

    $i = 0
    while ($True) {
        $i++
        $result = (Invoke-Command -ScriptBlock $ScriptBlock *>&1)
        Start-Sleep -Seconds $Seconds
        Write-Host -NoNewline "`e[2J"
        Write-Host -NoNewline "`e[1;1H"
        Write-Host "Iteration ${i}:"
        Write-Host
        foreach ($s in $result) {
            Write-Host $s
        }
    }
}

$global:PSDefaultParameterValues["Watch-Command:Seconds"] = 1

# ------------------------------------------------------------------------------
# ----- Measure size -----------------------------------------------------------

function Measure-Size() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory = $true, ValueFromPipeline = $true)]
        [Object[]]$Path
    )
    PROCESS {
        foreach ($P in $Path) {
            if ($P -is [string]) {
                $Item = Get-Item -Path $P
            } else {
                $Item = Get-Item -LiteralPath $P
            }
            $DeepSize = (Get-ChildItem -Recurse -Force -LiteralPath $Item | Measure-Object -Property Length -Sum).Sum
            Add-Member -Force -InputObject $Item -NotePropertyName DeepSize -NotePropertyValue $DeepSize
            Write-Output $Item
        }
    }
}

# ------------------------------------------------------------------------------
# ----- Guix Profiles ----------------------------------------------------------

function Initialize-GProfile() {
    [CmdletBinding()]
    param(
        [switch]$Force
    )
    if ((Test-Path "$PWD/manifest.scm") -and -not $Force) {
        Write-Error "manifest.scm already exists. Use -Force to overwrite it."
        return
    }
    @"
(use-modules
 ((gnu packages man)                              #:prefix man:))

(define the-manifest
  (packages->manifest
   ``(,man:man-db)))

the-manifest
"@ > "$PWD/manifest.scm"
    Write-Host "manifest.scm created!"
    Update-GProfile
}

function Update-GProfile() {
    [CmdletBinding()]
    param()
    if (-not (Test-Path -PathType Container -Path "$PWD/.profile")) {
        New-Item -Type Directory -Path "$PWD/.profile" > $null
    }
    guix package -p "$PWD/.profile/profile" -m  "$PWD/manifest.scm"
}

function Restore-GProfile() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory = $false)]
        [String]$Generation
    )
    if (-not $Generation) {
        guix package -p "$PWD/.profile/profile" --roll-back
    } else {
        guix package -p "$PWD/.profile/profile" -S "$Generation"
    }
}

function Clear-GProfile() {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory = $false)]
        [String]$Generation
    )
    guix package -p "$PWD/.profile/profile" -d "$Generation"
}

function Get-GProfileGenerations() {
    [CmdletBinding()]
    param()
    guix package -p "$PWD/.profile/profile" -l
}
