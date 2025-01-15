# HOW TO RUN R SCRIPT :

## 1. GIT CLONE
Steps:

- Open a terminal (or command prompt).<br>
- Enter the following command to clone the repository : <br> 
   - git clone https://github.com/your-username/INF6027.git.git

- Navigate to the cloned repository directory: <br>
    - cd INF6027

## 2. Switch Git Branch (if applicable)

If the repository has multiple branches (e.g., main, dev), ensure you are on the correct branch for the project. <br>
Steps :

- View all branches: <br>
   - git branch -a
- Switch to the desired branch (e.g., dev) <br>
   - git checkout dev

## 3. Open and Run Scripts in RStudio <br>
Steps: <br>

Launch RStudio.

Open the project:

- Navigate to File > Open Project in RStudio. <br>
- Select the .Rproj file within the cloned repository folder. <br>
- Load the required R scripts: <br>

   - Navigate to the /code/ folder and open the script you want to execute 
     (e.g., data_preprocessing.R, analysis.R).
- Install dependencies:

Run the following command in the RStudio console to install required packages: <br>

install.packages(c("tidyverse", "pheatmap", "caret", "randomForest"))

- Run the script:


