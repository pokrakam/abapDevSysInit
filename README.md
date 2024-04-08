# abapDevSysInit
ABAP Development System Configurator

# Purpose
To automate the repetitive tasks in setting up and personalizing a new 
ABAP Developer Edition or Trial system.

Preconfigured setups that can be automated: 
- Installing abapGit (Standalone edition)
- Installing GitHub SSL certificate (thanks to @sandraros and @mbtools)
- Setting up RFC Destination with GitHub Personal Access Token
- Creating abapGit User Exit to log into GitHub with token
- Pulling a list of repos
- ...more to come

# Usage

Pull with abapGit if installed, else just copy/paste `ZDEVSYSINIT` into a Z-report.

If you don't intend to push/pull updates, edit parameters in `main->constructor`. 
Otherwise, create include `zdevsysinit_params` outside the package, and copy the 
initialization parameter section from `main->constructor` and populate with 
appropriate values.

Do not put `zdevsysinit_params` in the same package as this may include personal 
information that should not be published on GitHub. Instead, save a copy on your 
laptop on in a Gist to reuse the next time you set up a new dev/trial system. 
It is highly recommended not to put tokens into the source code, or to delete 
them once the RFC destination has been created.
