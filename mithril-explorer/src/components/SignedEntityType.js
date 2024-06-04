import React, { useEffect, useState } from "react";
import { ListGroup, Table } from "react-bootstrap";

export default function SignedEntityType({ signedEntityType, table = false }) {
  const [entityName, setEntityName] = useState("");
  const [beacon, setBeacon] = useState({});

  useEffect(() => {
    let type_name = Object.keys(signedEntityType).at(0);
    setEntityName(type_name);

    if (type_name === "MithrilStakeDistribution" || type_name === "CardanoStakeDistribution") {
      setBeacon({
        epoch: signedEntityType[type_name],
      });
    } else if (type_name === "CardanoTransactions") {
      setBeacon({
        epoch: signedEntityType[type_name][0],
        block_number: signedEntityType[type_name][1],
      });
    } else {
      setBeacon(signedEntityType[type_name] ?? {});
    }
  }, [signedEntityType]);

  return table ? (
    <Table className="mb-3">
      <tbody>
        <tr>
          <td colSpan={2}>
            <h6>{entityName}</h6>
          </td>
        </tr>
        {Object.entries(beacon).map(([key, value]) => (
          <tr key={key}>
            <td>
              <em>{key}:</em>
            </td>
            <td>{value}</td>
          </tr>
        ))}
      </tbody>
    </Table>
  ) : (
    <ListGroup>
      <ListGroup.Item>
        <h6>{entityName}</h6>
      </ListGroup.Item>
      <ListGroup.Item>
        <ListGroup horizontal="xxl">
          {Object.entries(beacon).map(([key, value]) => (
            <ListGroup.Item key={key}>
              {key}: {value}
            </ListGroup.Item>
          ))}
        </ListGroup>
      </ListGroup.Item>
    </ListGroup>
  );
}
