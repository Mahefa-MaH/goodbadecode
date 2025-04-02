-- Good PL/SQL Code:  Using pipelined table functions for efficient data processing.

CREATE OR REPLACE TYPE number_table AS TABLE OF NUMBER;
/

CREATE OR REPLACE PIPELINED FUNCTION get_numbers(p_limit NUMBER)
  RETURN number_table PIPELINED
IS
  v_num NUMBER := 1;
BEGIN
  WHILE v_num <= p_limit LOOP
    PIPE ROW(v_num);
    v_num := v_num + 1;
  END LOOP;
  RETURN;
END;
/


-- Bad PL/SQL Code:  Inefficient cursor loop with implicit cursor

CREATE OR REPLACE PROCEDURE inefficient_process IS
  CURSOR c1 IS SELECT * FROM employees;
BEGIN
  FOR emp_rec IN c1 LOOP
    UPDATE employees SET salary = salary * 1.1 WHERE employee_id = emp_rec.employee_id;
    COMMIT; --Commit in loop, bad for performance
  END LOOP;
END;
/


-- Good PL/SQL Code: Exception Handling and logging
CREATE OR REPLACE PROCEDURE  safe_process (p_emp_id IN employees.employee_id%type)
IS
  v_salary employees.salary%type;
BEGIN
  SELECT salary INTO v_salary FROM employees WHERE employee_id = p_emp_id;
  --Perform operation
EXCEPTION
  WHEN NO_DATA_FOUND THEN
    DBMS_OUTPUT.PUT_LINE('Employee not found: ' || p_emp_id);
  WHEN OTHERS THEN
    DBMS_OUTPUT.PUT_LINE('An error occurred: ' || SQLERRM);
END;
/

-- Bad PL/SQL Code: No error handling, potential for silent failures.
CREATE OR REPLACE PROCEDURE unsafe_process (p_emp_id IN employees.employee_id%type)
IS
BEGIN
  UPDATE employees SET salary = salary * 1.1 WHERE employee_id = p_emp_id;
END;
/

