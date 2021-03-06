version =  3.7
services_dev_image =  geiser/r-studio:latest
services_dev_container_name =  2020-validation-QATeDI_app
services_dev_restart =  unless-stopped
services_dev_tty =  true

PREFIX = /cygdrive/d/Users/gcc/Workspace/2020-validation-QATeDI
APP_NAME = 2020-validation-QATeDI

DD = "/cygdrive/c/Program Files/Docker/Docker/resources/bin/docker"
DC = "/cygdrive/c/Program Files/Docker/Docker/resources/bin/docker-compose"


# HELP: This will output the help for each task
.PHONY: help

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

.DEFAULT_GOAL := all


all: pull run ## Pull and build the images to the project

pull: ## Pull images from docker.io to the project
	@$(DD) pull geiser/r-studio:latest

clean: ## Remove containers and images related to the project
	@$(DC) -p $(APP_NAME) down --remove-orphans --rmi all 2>/dev/null \
	&& echo 'Image(s) for "$(APP_NAME)" removed.' \
	|| echo 'Image(s) for "$(APP_NAME)" already removed.'
	@rm Makefile

run: ## Run the r-studio with the project in http://127.0.0.0.1:8787/ 
	$(DC) -p $(APP_NAME) up

down: ## Down r-studio from the container of service
	$(DC) -p $(APP_NAME) down

# HELPERS
prune: ## clean all that is not actively used
	$(DD) system prune -af
