import {useDispatch, useSelector} from "react-redux";
import {Button, Col, Form, InputGroup} from "react-bootstrap";
import {setUpdateInterval, toggleAutoUpdate} from "../../store/settingsSlice";

export default function IntervalSetter(props) {
  const autoUpdate = useSelector((state) => state.settings.autoUpdate);
  const updateInterval = useSelector((state) => state.settings.updateInterval);
  const dispatch = useDispatch();

  return (
    <Form.Group as={Col} className={props.className}>
      <Form.Label>Update Interval:</Form.Label>
      <InputGroup>
        <Button type="button" onClick={() => dispatch(toggleAutoUpdate())} variant={autoUpdate ? "primary" : "success"}>
          {autoUpdate ? "Pause ⏸" : "Resume ▶"}
        </Button>
        <Form.Select value={updateInterval} onChange={(e) => dispatch(setUpdateInterval(e.target.value))}>
          <option value={1000}>1 seconds</option>
          <option value={5000}>5 seconds</option>
          <option value={10000}>10 seconds</option>
        </Form.Select>
      </InputGroup>
    </Form.Group>
  );
}
