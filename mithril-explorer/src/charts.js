import { toAda } from "./utils";

// Compute a ChartJs dataset for the stake shapes of the given signers, they
// must have a ".stake" property in order for this method to work.
function computeStakeShapesDataset(signersList) {
  const signers = signersList ?? [];

  const labels = [
    "< 1M₳",
    "≥ 1M₳ < 10M₳",
    "≥ 10M₳ < 25M₳",
    "≥ 25M₳ < 50M₳",
    "≥ 50M₳ < 75M₳",
    "≥ 75M₳ < 100M₳",
    "≥ 100M₳",
  ];

  const toMillionAda = (lovelace) => lovelace / 1000000000000;
  const stakes = signers.map((r) => toMillionAda(r.stake));

  let data = [
    stakes.filter((stake) => stake < 1).length,
    stakes.filter((stake) => stake >= 1 && stake < 10).length,
    stakes.filter((stake) => stake >= 10 && stake < 25).length,
    stakes.filter((stake) => stake >= 25 && stake < 50).length,
    stakes.filter((stake) => stake >= 50 && stake < 75).length,
    stakes.filter((stake) => stake >= 75 && stake < 100).length,
    stakes.filter((stake) => stake > 100).length,
  ];

  return {
    labels: labels,
    datasets: [
      {
        label: "Number of signers",
        data: data,
      },
    ],
  };
}

// Compute a ChartJs dataset for the signers weight of the given signers, they
// must have a ".stake"  and a ".party_id" properties in order for this method to work.
function computeSignersWeightDataset(signersList) {
  const signers = signersList ?? [];

  return {
    labels: signers.map((r) => r.party_id),
    datasets: [
      {
        label: "Stake (₳)",
        data: signers.map((r) => toAda(r.stake)),
      },
    ],
  };
}

function setChartJsDefaults(chartJs) {
  const backgroundColor = [
    "rgba(255, 99, 132, 0.2)",
    "rgba(255, 159, 64, 0.2)",
    "rgba(255, 205, 86, 0.2)",
    "rgba(75, 192, 192, 0.2)",
    "rgba(54, 162, 235, 0.2)",
    "rgba(153, 102, 255, 0.2)",
    "rgba(201, 203, 207, 0.2)",
  ];
  const borderColor = [
    "rgb(255, 99, 132)",
    "rgb(255, 159, 64)",
    "rgb(255, 205, 86)",
    "rgb(75, 192, 192)",
    "rgb(54, 162, 235)",
    "rgb(153, 102, 255)",
    "rgb(201, 203, 207)",
  ];

  // Global - hide charts title
  chartJs.defaults.plugins.legend.display = false;

  // Pie chart
  chartJs.defaults.elements.arc.backgroundColor = backgroundColor;
  chartJs.defaults.elements.arc.borderColor = borderColor;
  chartJs.defaults.elements.arc.borderWidth = 1;

  // Bar chart
  chartJs.defaults.elements.bar.backgroundColor = backgroundColor;
  chartJs.defaults.elements.bar.borderColor = borderColor;
  chartJs.defaults.elements.bar.borderWidth = 1;
}

export { computeSignersWeightDataset, computeStakeShapesDataset, setChartJsDefaults };
