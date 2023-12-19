# Manually publish packages to npm registry.

## Introduction

From time to time, we may need to publish manually packages to [npm ristry](https://npmjs.com/) registry.

## Run the associated 'Manual npm publication' GitHub Actions workflow

Go to the page of the workflow with your browser: [Manual npm publication](https://github.com/input-output-hk/mithril/actions/workflows/manual-publish-npm.yml)

Then, click on the **Run workflow** button:

![Run workflow button](./img/run-workflow-button.png)

Then fill the form to manually run the workflow:

![Run workflow form](./img/run-workflow-form.png)

> [!CAUTION]
> It is highly recommended to run with the **Dry run** option checked at first and make sure that the process works as expected.


The result should look like this in the GitHub Actions:

![Run workflow result](./img/run-workflow-result.png)
