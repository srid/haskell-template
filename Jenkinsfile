// We use https://github.com/juspay/jenkins-nix-ci

pipeline {
    agent any
    stages {
        stage ('Build') {
            steps {
                nixBuildAll ()
            }
        }
    }
}
