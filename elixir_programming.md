**Title:** Elixir Process Management: Robust vs. Leaky

**Summary:**  The key difference lies in handling process supervision: robust code leverages Elixir's built-in supervision trees for fault tolerance, while leaky code lacks proper supervision, leading to uncontrolled crashes and resource exhaustion.

**Good Code:**

```elixir
defmodule MySupervisor do
  use Supervisor

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: :my_supervisor)
  end

  def init(arg) do
    children = [
      worker(MyWorker, [arg]),
      supervisor(MyChildSupervisor, [])
    ]
    Supervisor.init(children, strategy: :one_for_one)
  end
end

defmodule MyWorker do
  def init(arg) do
    # ... worker logic ...
    {:ok, arg} 
  end

  def handle_call(request, state) do
     #...handle call...
     {:reply, :ok, state}
  end

  def handle_info(message, state) do
    #...handle info...
    {:noreply, state}
  end
  def handle_cast(message, state) do
    #...handle cast...
    {:noreply, state}
  end
  def terminate(_reason, _state) do
    :ok
  end
end

defmodule MyChildSupervisor do
  use Supervisor
  def start_link(_) do
    Supervisor.start_link(__MODULE__, [], name: :my_child_supervisor)
  end
  def init(_) do
    children = [worker(MySubWorker, [])]
    Supervisor.init(children, strategy: :one_for_one)
  end
end

defmodule MySubWorker do
  def init(_) do
      {:ok, :initialized}
  end
  def handle_info(:stop, state) do
      {:stop, :normal, state}
  end

end


# Starting the supervisor
{:ok, pid} = MySupervisor.start_link([])
#Send a message
GenServer.call(MyWorker, :get_data)
#Stop the supervisor
Supervisor.terminate(:my_supervisor, :normal)
```

**Bad Code:**

```elixir
defmodule MyBadWorker do
  def start do
    Task.start(fn ->
      try do
        # ... some potentially failing code ...
        raise "Something went wrong!"
      rescue
        e -> IO.puts("Error: #{inspect(e)}") #No proper handling
      end
    end)
  end
end

MyBadWorker.start()
```


**Key Takeaways:**

* **Fault Tolerance:** The good code utilizes Elixir's `Supervisor` to manage processes. If one worker crashes, the supervisor restarts it, preventing application failure. The bad code lacks supervision, resulting in a silent failure.
* **Resource Management:**  The supervisor in the good code ensures that resources are properly released even after failures. The bad code may leak resources if exceptions are not handled correctly.
* **Maintainability:** The structured approach of the good code makes it easier to understand, debug, and maintain.  The bad code is harder to reason about and debug, especially in larger applications.
* **Error Handling:** The good code provides a framework for structured error handling with the use of different function calls like `handle_call`, `handle_cast`, `handle_info` and `terminate` in `GenServer` and `Supervisor`. The bad code only has basic error handling and no recovery strategy.
* **Scalability:** The good code’s use of supervision trees scales gracefully as the application grows; the bad code's approach becomes increasingly difficult to manage with more workers.

