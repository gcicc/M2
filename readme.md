# Instructions on use of MARCS
 
Prerequisites: 
- a user id with access to "MARCS" git repo and "MARCS" Jira project
- an installation of R v4.3.3 and RStudio 2024.04.2 Build 764
- an installation of Rtools for 4.3.x, see https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html

To execute analyses,
1. Clone "MARCS" repo.
2. Add your individual definitions of local_sync_dir and repo_clone (leaving but commenting out the others).
3. Execute "global-config.R" at least once but whenever you want a different definition of "studies" 
4. Execute "execute-analyses-across-studies.R", adjusting comments for "analysis_types" to suit your purpose.
   Note, do not change the order of the types, just adjust using comments.

To change code, 
1. Move an approved Jira issue to "IN PROGRESS" and make a branch named with issue key prefix.
2. Commit early and often, occasionally seek peer review using a pull request.
   Note that all commit messages shoudl be descriptive, and bear the Jira issue key as prefix.
3. On acceptance of final peer review, merge the branch and delete it.
4. Set Jira issue to "DONE".
