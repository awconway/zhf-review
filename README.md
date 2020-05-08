# Accuracy and precision of zero-heat-flux temperature measurements with the 3M^TM^ Bair Hugger^TM^ Temperature Monitoring System: A systematic review and meta-analysis

## Reproducibility

The statistical anlyses requires various packages to be installed, and may not work properly if package versions have changed. Therefore, a [Docker image is provided](https://hub.docker.com/repository/docker/awconway/hfnosedrct) to run the code reproducibly.

### Run Docker locally

**If you already have [docker](https://docs.docker.com/install/) installed**

- Run the following in a terminal (substituting in a user name and password):

```
docker run -d -p 8787:8787 -e USER=<user> -e PASSWORD=<password> awconway/zhf-review
```

- Open a web browser and go to: localhost:8787
- Enter your username and password to enter an RStudio session.
- Create a new project from version control (File > New project > Version Control > Git > https://github.com/awconway/zhf-review.git )
- Open the `manuscript` folder, then open and knit the document `manuscript.Rmd` to completely reproduce the analysis of results presented in the publication as `manuscript.docx`.

There will be a pop-up window asking you to download the word document when it is ready.


### Run Docker on a Cloud

Instead of installing docker on your system you can run it on a remote server, such as [Digital Ocean](https://www.digitialocean.com). This [link](https://m.do.co/c/89cf8df06791) provides you with $100 free credit to use for a 60-day period. After signing up, follow these steps to run this project on a Digital Ocean droplet:

- Create a DigitalOcean droplet. Choose a server with Docker installed from the *Marketplace* menu and choose a size for your server (number of CPUs and amount of RAM). The default is a good choice.

- Select `User data` from the `Select additional options` section and enter the text as displayed below (substituting in a username and password).

```
#cloud-config
runcmd:
  - docker run -d -p 8787:8787 -e USER=<user> -e PASSWORD=<password> awconway/zhf-review
```

- Create the droplet.

- Wait a few minutes for the docker image to load into the server then open a web browser and type in the ip address of the droplet you just created followed by the port 8787 (e.g. ipaddress:8787).

- Follow the instructions for cloning the repository and running the analyses as outlined above.

- *Destroy the DigitalOcean droplet when finished inspecting the analyses.*

