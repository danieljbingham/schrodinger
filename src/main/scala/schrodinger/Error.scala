package schrodinger

case object CircuitBreakerOpenException extends Exception("Circuit breaker is open")
