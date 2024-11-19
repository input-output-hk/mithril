import { useDispatch, useSelector } from "react-redux";
import { Button, Col, Form, InputGroup, OverlayTrigger, Tooltip } from "react-bootstrap";
import { setUpdateInterval, changeRefreshSeed } from "@/store/settingsSlice";

export default function IntervalSetter(props) {
  const updateInterval = useSelector((state) => state.settings.updateInterval);
  const dispatch = useDispatch();

  return (
    <Form.Group as={Col} {...props}>
      <InputGroup>
        <Button type="button" onClick={() => dispatch(changeRefreshSeed())} variant="primary">
          Refresh
        </Button>
        <OverlayTrigger overlay={<Tooltip>Set auto-refresh interval</Tooltip>}>
          <Form.Select
            aria-label="Auto refresh interval"
            value={updateInterval}
            onChange={(e) => dispatch(setUpdateInterval(parseInt(e.target.value)))}>
            <option value={undefined}>Off</option>
            <option value={1000}>1 second</option>
            <option value={5000}>5 seconds</option>
            <option value={10000}>10 seconds</option>
          </Form.Select>
        </OverlayTrigger>
      </InputGroup>
    </Form.Group>
  );
}
