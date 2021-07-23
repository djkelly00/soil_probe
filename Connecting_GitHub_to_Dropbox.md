#How to Connect GitHub to Dropbox #

##1. Clone the GitHub repository to a folder NOT in your Dropbox using GitHub Desktop ##
 - Go to 'Current Repository' and select the arrow to search and add the correct repository
 - Designate the location/file path for the cloned repository folder

##2. One member of the team needs to clone the repository to a shared Dropbox folder#
> *Only one person needs to complete the following steps*
> 
 - Create a shared folder in Dropbox
 
    cd "C:/Users/researcher/Dropbox/name_of_folder"

  - Go to the main page of the repository on GitHub. Select the down arrow on the green 'Code' button and copy the HTTPS URL 
  - Back in the command prompt, type the command and URL (changed from the example):

    `git clone https://github.com/djkelly00/soil_probe.git`

 - You should now have two folders - one locally and one on Dropbox. Always make edits to the local version, push to the cloud, and then pull edits to the Dropbox folder


----------

##How to Update the Dropbox Clone##

##1. Using Git Bash, navigate to the Dropbox folder ##

    cd "C:/Users/researcher/Dropbox/name_of_folder/name_of_cloned_repository"





