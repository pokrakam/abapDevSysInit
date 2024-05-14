# abapDevSysInit
ABAP Development System Configurator. 
Work in progress, expect bugs and changes, may break stuff, use at own risk.

# Purpose
To automate the repetitive tasks in setting up and personalizing a new 
ABAP Developer Edition or Trial system.

Setups and actions that can be configured: 
- Set up RFC Destination with GitHub Personal Access Token
- Install GitHub SSL certificate (thanks to @sandraros and @mbtools)
- Install abapGit standalone edition
- Customize user profile (Name, localization, email address)
- Pull a list of repos

See projects page to see what's planned

# Usage

The design is to have everything in one report for ease of installing by copy/paste
as abapGit may not be available. Configuration is also done by copy/paste and editing 
an ABAP include.

Pull with abapGit if installed, else just create report `zdevsysinit` and copy/paste 
the source from [zdevsysinit.prog.abap](https://raw.githubusercontent.com/pokrakam/abapDevSysInit/main/src/zdevsysinit.prog.abap).

If you don't intend to push/pull updates, edit parameters in `main->constructor`. 
Otherwise, create include `zdevsysinit_params` outside the package, and copy the 
initialization parameter examples from [zdevsysinit_sample.abap](https://raw.githubusercontent.com/pokrakam/abapDevSysInit/main/zdevsysinit_sample.abap) 
and update with appropriate values.

Do not put `zdevsysinit_params` in the same package as this may include personal 
information that should not be published on GitHub. Instead, save a copy on your 
laptop on in a Gist to reuse the next time you set up a new dev/trial system. 
It is highly recommended not to put tokens into the source code, or to delete 
them once the RFC destination has been created.
