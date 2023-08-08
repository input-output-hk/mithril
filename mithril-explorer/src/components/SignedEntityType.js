import React, {useEffect, useState} from 'react';
import {ListGroup} from "react-bootstrap";

export default function SignedEntityType({signedEntityType}) {
  const [entityName, setEntityName] = useState("");
  const [beacon, setBeacon] = useState({});

  useEffect(() => {
    let type_name = Object.keys(signedEntityType).at(0);
    setEntityName(type_name);

    if (type_name === "MithrilStakeDistribution" || type_name === "CardanoStakeDistribution") {
      setBeacon({
        epoch: signedEntityType[type_name]
      })
    } else {
      setBeacon(signedEntityType[type_name] ?? {});
    }
  }, [signedEntityType]);

  return (
    <ListGroup>
      <ListGroup.Item>
        <h6>{entityName}</h6>
      </ListGroup.Item>
      <ListGroup.Item>
        <ListGroup horizontal="xxl">
          {Object.entries(beacon).map(([key, value]) =>
            <ListGroup.Item key={key}>{key}: {value}</ListGroup.Item>
          )}
        </ListGroup>
      </ListGroup.Item>
    </ListGroup>
  );
}
