# Synapse DCC Utilities

Code for managing data coordinating operations (e.g., development of the CSBC/PS-ON Knowledge Portal and individual Center pages) for Sage-supported communities through Synapse. Currently this package is able to:
* Build tables and charts to summarize annotations from a view
* Build a DCC dashboard to summarize multiple projects
* Provide tools/scripts to handle metadata management (e.g to/from GEO)
* Pull publications from PubMed
* Build projects
* Invite members to a team

What to see examples? Go to [config/targets.md](config/targets.md)

## Getting Started
There are a few ideas to get started using this package

### Building visualizations from a file view
Here we can use a file view with annotations to create multiple charts of visualizations

### Summarizing projects by dashboard
Here we can summarize activity by project, date added, data type, etc.

## Contributions
Code in this respository should be validated before commits are pushed, pull requests made, etc. We can use pre-commit hooks to do this. 
1. [Install the awslabs git-secrets script](https://github.com/awslabs/git-secrets) to your dev environment
2. Make sure the `git-secrets` script is in the $PATH of your shell 
3. Run `git secrets --install && git secrets --register-aws` while in the local repository
4. `git-secrets` will run before commits but will only output when it finds a problem


