#! /usr/bin/env node

const sgMail = require("@sendgrid/mail");
sgMail.setApiKey(process.env.SENDGRID_API_KEY);

const msg = {
  to: `${process.env.SENDGRID_MAIL_TO}`,
  from: `${process.env.SENDGRID_MAIL_FROM}`,
  subject: "🚨 Nightly Workflow Failed",
  text: `
    Nightly Trigger workflow has failed, see details:

    - Workflow: ${process.env.GITHUB_WORKFLOW}
    - Repository: ${process.env.GITHUB_REPOSITORY}
    - Run Number: ${process.env.GITHUB_RUN_NUMBER}
    - Logs: ${process.env.GITHUB_SERVER_URL}/${process.env.GITHUB_REPOSITORY}/actions/runs/${process.env.GITHUB_RUN_ID}
  `,
  html: `
    <p>Nightly Trigger workflow has failed, see details:</p>
    <ul>
      <li><strong>Workflow :</strong> ${process.env.GITHUB_WORKFLOW}</li>
      <li><strong>Repository :</strong> ${process.env.GITHUB_REPOSITORY}</li>
      <li><strong>Run Number :</strong> #${process.env.GITHUB_RUN_NUMBER}</li>
      <li><strong>Logs :</strong> <a href="${process.env.GITHUB_SERVER_URL}/${process.env.GITHUB_REPOSITORY}/actions/runs/${process.env.GITHUB_RUN_ID}">Check logs</a></li>
    </ul>
  `,
};

sgMail
  .send(msg)
  .then(() => console.log("Mail sent successfully"))
  .catch((error) => console.error(error.toString()));
