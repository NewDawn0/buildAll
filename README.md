# 🏗️ build-all – Nix flakes on steroids

> A no-nonsense CLI that builds **every damn output** from your flake—packages, devShells, apps—with zero patience for your excuses.

> No interactive prompts. No "maybe later". Just raw, unfiltered `nix build`.

---

## 🚀 Installation

### The Nix Way

Edit your system Flake to include the following

```bash
{
    inputs.build-all.url = "github:NewDawn0/buildAll";
    # ...
    pkgs = import nixpkgs {
        # ...
        overlays = [ inputs.build-all.overlays.default ];
    }
    # ...
    environment.systemPackages = [ pkgs.build-all ];
}
```

### The "I Like Pain" Way (manual)

```bash
git clone https://github.com/NewDawn0/buildAll
cd buildAll
cabal build
```

### For the Masochists

⚠️ Warning: The missing dependencies will hurt more than your pride

```bash
git clone https://github.com/NewDawn0/buildAll
cd buildAll
ghc -O2 -o build-all src/*.hs
```

## 📖 Usage

```bash
build-all [FLAKE_REF]
```

### Options

- `-a` | `--all` — Build all outputs
  > Like a wrecking ball on your CPU)
- `-d` | `--devShell` — Build the devShell
  > For when you want only half the suffering
- `-p` | `--packages` — Build the packages
  > For when you want the other half

### Examples

```bash
# Build ALL outputs from current directory flake
build-all .#

# Build only packages from a GitHub flake
build-all -p github:someuser/someflake
```

## 🤔 Why This Exists

Because typing `nix build .#packages.x86_64-linux.foo` 47 times makes you question life choices.

Meanwhile:

`build-all` says:

> 🔨 "Give me a flake. I'll burn it all down."
>
> ⚡ Zero configuration. Zero mercy.

## 🧩 How It Works

1. Detects all flake outputs (like a bloodhound on Red Bull)
2. Builds them in order (with progress bars because we're not animals)
3. Prints results (green for success, red for your failures)

No magic. No Docker. Just pure Nix-grade determinism.

## ⚖️ License

MIT – Do whatever, just don't blame me when your CI bill spikes.

## 🔥 Final Words

> _"Modern devtools ship 300MB node_modules to print 'Hello World'.
> This ships 3MB of pure Nix/Haskell fury."_

💻 Stop building manually. Start building everything.

![finalGif](https://media.giphy.com/media/v1.Y2lkPTc5MGI3NjExcDZ6Y2F4d2Z0b2NtZ2VjY2V6dGJ6YnR5dG5mM2R6eGZ6Z2Z5bW5zbiZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/xT5LMHxhOfscxPfIfm/giphy.gif)

(Actual performance may vary. Batteries not included. May contain traces of sarcasm.)
