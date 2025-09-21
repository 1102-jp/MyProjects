
-- Borrower distribution by age and income bracket
SELECT 
    CASE 
        WHEN person_age < 25 THEN '18-24'
        WHEN person_age BETWEEN 25 AND 34 THEN '25-34'
        WHEN person_age BETWEEN 35 AND 44 THEN '35-44'
        WHEN person_age BETWEEN 45 AND 54 THEN '45-54'
        ELSE '55+' 
    END AS age_group,
    CASE 
        WHEN person_income < 30000 THEN '<30K'
        WHEN person_income BETWEEN 30000 AND 60000 THEN '30K-60K'
        WHEN person_income BETWEEN 60001 AND 100000 THEN '60K-100K'
        ELSE '100K+' 
    END AS income_bracket,
    COUNT(*) AS total_borrowers
FROM loans
GROUP BY age_group, income_bracket
ORDER BY age_group, income_bracket;



-- Average loan amount by intent of loan
select loan_intent,
        round(avg(loan_amnt), 2) as avg_loan_amount,
        count(*) as total_loans
from loans
group by loan_intent
order by avg_loan_amount desc;



-- Default rate by loan grade
select loan_grade,
        count(*) as total_loans,
        sum(loan_status) as defaults,
        round(100.0 * sum(loan_status)/count(*), 2) as default_rate

from loans
group by loan_grade
order by default_rate desc;



-- High Risk Borrowers (those with loan_income > 40%)
select person_age, person_income, loan_amnt, loan_percent_income, loan_status
from loans
where loan_percent_income > 0.4;



-- Default rate by employment length

select person_emp_length, count(*) as total_loans, sum(loan_status) as defaults,
        round(100.0 * sum(loan_status)/count(*), 2) as default_rate
from loans
group by person_emp_length
order by person_emp_length;


